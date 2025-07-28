import "cypress-real-events";

describe("app", () => {
  beforeEach(() => {
    cy.visit("/");
  });

  it("Starts", () => {});

  it("Has 10 tabs", () => {
    cy.get("ul.teal-modules-tree a.module-button", { timeout: 30000 }).should(
      "have.length",
      9
    );
  });

  it("Navigates to all tabs without error", () => {
    cy.get("ul.teal-modules-tree a.module-button", {
      timeout: 30000,
    }).then(($moduleButtons) => {
      cy.wrap($moduleButtons).each(($el) => {
        cy.wrap($el).as("tealTab");

        cy.get("@tealTab").then(($el2) => {
          cy.log(`Navigating to: ${$el2[0].innerText}`);
        });

        cy.contains(".dropdown.nav-item-custom", "Module").realHover();
        cy.get(".dropdown-menu").should("have.class", "show");
        cy.get("@tealTab").click();
        cy.get("@tealTab").invoke("attr", "href").as("hrefTab");
        cy.contains(".dropdown.nav-item-custom", "Module").trigger("mouseout");
        cy.get("html").not(".shiny-busy");
      });
    });
  });
});
