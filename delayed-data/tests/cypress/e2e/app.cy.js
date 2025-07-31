import "cypress-real-events";

describe("app", () => {
  beforeEach(() => {
    cy.visit("/");
  });

  it("Starts", () => {});

  it("Has 1 tab", () => {
    cy.get("ul.teal-modules-tree a.module-button", { timeout: 30000 }).should(
      "have.length",
      1
    );
  });

  it("Shows the data popup during initialization", () => {
    cy.get(".modal-content").should("be.visible");
  });

  it("The Dismiss button in the data popup is disabled", () => {
    cy.get(".modal-content .modal-footer button").should("be.disabled");
  });

  it("The Dismiss button in enabled after load datasets is clicked", () => {
    cy.contains("button", "Load Datasets").click();
    cy.get(".modal-content .modal-footer button").should("not.be.disabled");
  });

  it("After closing the popup, the module is loaded without any errors", () => {
    cy.contains("button", "Load Datasets").click();
    cy.get(".modal-content .modal-footer button").click();
    cy.waitForShinyStabilityAndCheckError();
  });
});
