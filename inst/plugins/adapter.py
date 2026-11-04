# The stock executable loads this adapter through PythonPlugin:Instance.
import atexit
import ctypes
import json
import os
import sys
import threading
import types
import uuid
from pyenergyplus.plugin import EnergyPlusPlugin


# Retain one protected R runtime for all compatible plugin modules in this process.
class EmbeddedR:
    # Initialize one R interpreter on the plugin thread and retain its native libraries.
    def __init__(self, config):
        self.owner = threading.get_ident()
        self.config = config
        os.environ['R_HOME'] = config['r_home']
        os.environ['R_LIBS'] = os.pathsep.join(config['libraries'])
        self.r = ctypes.CDLL(config['lib_r'], mode=ctypes.RTLD_GLOBAL)
        if ctypes.c_void_p.in_dll(self.r, 'R_GlobalEnv').value:
            raise RuntimeError('An unmanaged R interpreter is already initialized')
        ctypes.c_int.in_dll(self.r, 'R_SignalHandlers').value = 0
        # Start from recorded libraries without executing user or site startup profiles.
        argv = (ctypes.c_char_p * 3)(b'eplusr-plugin', b'--silent', b'--vanilla')
        self.r.Rf_initEmbeddedR.argtypes = [ctypes.c_int, ctypes.POINTER(ctypes.c_char_p)]
        self.r.Rf_initEmbeddedR.restype = ctypes.c_int
        if self.r.Rf_initEmbeddedR(3, argv) < 0:
            raise RuntimeError('Embedded R initialization failed')
        ctypes.c_size_t.in_dll(self.r, 'R_CStackLimit').value = ctypes.c_size_t(-1).value
        self.bridge = ctypes.CDLL(config['package'])
        bootstrap = self.bridge.eplusr_plugin_bootstrap
        bootstrap.argtypes = []
        bootstrap.restype = ctypes.c_int
        if bootstrap():
            raise RuntimeError('Could not load eplusr in embedded R')
        self.dispatch = self.bridge.eplusr_plugin_dispatch
        self.dispatch.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p, ctypes.c_void_p]
        self.dispatch.restype = ctypes.c_int

    # Reject foreign threads and mismatched exports before any R API call.
    def call(self, config, event, instance, state):
        if threading.get_ident() != self.owner:
            raise RuntimeError('R plugin delivery on a foreign thread')
        for field in ('r_home', 'lib_r', 'package', 'libraries'):
            if config[field] != self.config[field]:
                raise RuntimeError('R plugin exports use incompatible R/package installations')
        return self.dispatch(config['configuration'].encode('utf-8'), event.encode('utf-8'), instance.encode('ascii'), state)


# Each instance borrows the executable state and gets a distinct serialized R context.
def initialize(self):
    EnergyPlusPlugin.__init__(self)
    self.instance = uuid.uuid4().hex
    self.finished = False
    self.state = None
    with open(CONFIG, encoding='utf-8') as stream:
        self.config = json.load(stream)
    if os.path.realpath(os.path.dirname(self.api.api._name)) != os.path.realpath(self.config['installation']):
        raise RuntimeError('R plugin export targets a different EnergyPlus installation')
    shared = sys.modules.get('__eplusr_embedded_r')
    if shared is None:
        shared = types.ModuleType('__eplusr_embedded_r')
        shared.runtime = EmbeddedR(self.config)
        sys.modules[shared.__name__] = shared
    self.runtime = shared.runtime
    atexit.register(self.finish)


# Finalization is ordinary-data-only because native teardown may already have happened.
def finish(self):
    if self.finished:
        return
    self.finished = True
    if self.state is not None:
        self.runtime.call(self.config, 'finalize', self.instance, self.state)


# Build each override directly in the generated class dictionary for EnergyPlus hook detection.
def make_hook(event):
    # Forward one native calling point and return EnergyPlus success/failure status.
    def hook(self, state):
        self.state = state
        try:
            result = self.runtime.call(self.config, event, self.instance, state)
            if result:
                self.api.runtime.issue_severe(state, 'eplusr R plugin failed: ' + self.instance)
            return result
        except BaseException as error:
            self.api.runtime.issue_severe(state, 'eplusr R plugin bridge failed: ' + str(error))
            return 1
    return hook


with open(CONFIG, encoding='utf-8') as stream:
    _configuration = json.load(stream)
_namespace = {'__init__': initialize, 'finish': finish}
for _event, _method in (_configuration['hooks'] or {}).items():
    _namespace[_method] = make_hook(_event)
# Bind before warmup even when the user's first selected hook runs later.
_namespace.setdefault('on_begin_new_environment', make_hook('begin_new_environment'))
globals()[CLASS] = type(CLASS, (EnergyPlusPlugin,), _namespace)
