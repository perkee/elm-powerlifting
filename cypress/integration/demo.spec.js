context("Demo", () => {
  /**
    For demo purpose, all tests are in the same file. Feel free to dispatch
    the following contexts in multiple *.spec.js files.
   */
  context("Visit the homepage", () => {
    beforeEach(() => {
      cy.visit("/");
    });

    it("display the content related to this URL", () => {
      cy.get("[data-test=title]").contains("Every Score Calculator");
    });
  });
});
