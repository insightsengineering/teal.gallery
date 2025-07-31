Cypress.Commands.add("waitForShinyIdle", (options = {}) => {
  const { timeout = 10000, checkInterval = 100, minStableTime = 200 } = options;

  cy.get("html", { timeout }).should(($html) => {
    expect($html).to.not.have.class("shiny-busy");
  });

  cy.wait(minStableTime);
  cy.get("html").should("not.have.class", "shiny-busy");
});
