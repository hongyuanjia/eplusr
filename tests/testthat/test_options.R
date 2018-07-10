describe("eplusr_option()", {
    # {{{
    it("works", {
        expect_error(eplusr_option(validate = TRUE), "Invalid option name found")
        expect_error(eplusr_option(validate_level = "wrong"))
        ops <- list(validate_level = "draft",
                    view_in_ip = FALSE,
                    save_format = "sorted")
        expect_equal(eplusr_option(ops), ops)
    })
    # }}}
})

