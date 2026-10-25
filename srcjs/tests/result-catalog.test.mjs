import test from "node:test";
import assert from "node:assert/strict";
import { reconcileResultFilters } from "../src/result-catalog.js";

// Model recorded rows where variables differ in keys, frequency and environment coverage.
const row = (name, reporting_frequency, key_value, environment_name, day_type) => ({ name, reporting_frequency, key_value, environment_name, day_type });
const catalog = { combinations: [
  row("Temperature", "Hourly", "Zone A", "Summer", "SummerDesignDay"),
  row("Temperature", "Hourly", "Zone B", "Summer", "SummerDesignDay"),
  row("Temperature", "Hourly", "Zone B", "Winter", "WinterDesignDay"),
  row("Temperature", "TimeStep", "Zone A", "Summer", "SummerDesignDay"),
  row("Heating", "TimeStep", "Zone B", "Winter", "WinterDesignDay")
] };

test("variable changes preserve valid children and remove incompatible choices", () => {
  const result = reconcileResultFilters(catalog, { name: ["Heating"], environment_name: ["Summer", "Winter"], reporting_frequency: ["Hourly", "TimeStep"], key_value: ["Zone A", "Zone B"], day_type: ["SummerDesignDay", "WinterDesignDay"] });
  assert.deepEqual(result.selected, { name: ["Heating"], environment_name: ["Winter"], reporting_frequency: ["TimeStep"], key_value: ["Zone B"], day_type: ["WinterDesignDay"] });
  assert.deepEqual(result.options.reporting_frequency, ["TimeStep"]);
  assert.deepEqual(result.options.key_value, ["Zone B"]);
  assert.equal(result.removed.length, 4);
});

test("environment changes remove only invalid day types and clearing restores the catalog", () => {
  const result = reconcileResultFilters(catalog, { name: ["Temperature"], environment_name: ["summer"], day_type: ["SummerDesignDay", "WinterDesignDay"] });
  assert.deepEqual(result.selected.day_type, ["SummerDesignDay"]);
  assert.deepEqual(result.options.day_type, ["SummerDesignDay"]);
  const cleared = reconcileResultFilters(catalog, { ...result.selected, environment_name: [] });
  assert.deepEqual(cleared.options.day_type, ["SummerDesignDay", "WinterDesignDay"]);
  assert.deepEqual(cleared.selected.day_type, ["SummerDesignDay"]);
});

test("frequency narrows keys and invalid-only choices become unrestricted with explicit feedback", () => {
  const result = reconcileResultFilters(catalog, { name: ["Temperature"], environment_name: ["Summer"], reporting_frequency: ["TimeStep"], key_value: ["Zone B"] });
  assert.deepEqual(result.options.key_value, ["Zone A"]);
  assert.deepEqual(result.selected.key_value, []);
  assert.deepEqual(result.removed, [{field: "key_value", values: ["Zone B"]}]);
});

test("multi-variable union admits only combinations with recorded data", () => {
  const result = reconcileResultFilters(catalog, { name: ["Heating", "Temperature"], reporting_frequency: ["Hourly"], key_value: ["Zone A", "Zone B"], environment_name: ["Winter"] });
  assert.deepEqual(result.selected.key_value, ["Zone B"]);
  assert.deepEqual(result.options.day_type, ["WinterDesignDay"]);
  assert.deepEqual(reconcileResultFilters(catalog, {}).options.name, ["Heating", "Temperature"]);
});
