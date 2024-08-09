describe("app", () => {
  beforeEach(() => {
    cy.visit("/");
  });

  it("Starts", () => {});

  it("Has 2 tabs", () => {
    cy.get(".nav.nav-pills a[data-bs-toggle=tab]", { timeout: 30000 }).should(
      "have.length",
      2
    );
  });

  it("Initializes with one disabled module", () => {
    cy.get(".nav.nav-pills a:not([data-value=teal_data_module])[disabled=disabled]", {
      timeout: 30000,
    }).should(
      "have.length",
      1
    );
  });

  it("Should enable the module after successful data pull", () => {
    // Cllick the button labled Load Datasets
    cy.contains('button', 'Load Datasets').click();
    cy.get(".nav.nav-pills a:not([data-value=teal_data_module])[disabled=disabled]", {
      timeout: 30000,
    }).should(
      "have.length",
      0
    );
  });

  it("Should be able to navigate to the module after successful data pull", () => {
    // Cllick the button labled Load Datasets
    cy.contains('button', 'Load Datasets').click();
    cy.contains('.nav.nav-pills a:not([data-value=teal_data_module])', 'example teal module').click();

    cy.get('.selectize-input').then(($input) => {
      const text = $input.text();
      console.log(text);
      expect(text).to.include('iris');
      expect(text).to.include('mtacars');
    });
  });

  it.only("Should be able to update the data using teal_data_module", () => {
    // Cllick the button labled Load Datasets
    cy.contains('button', 'Load Datasets').click();
    cy.contains('.nav.nav-pills a:not([data-value=teal_data_module])', 'example teal module').click();
    cy.get('.nav.nav-pills a[data-value=teal_data_module]').click();
    cy
    .get('#teal-data-content .selectize-input')
    .type('{backspace}{backspace}CO2{enter}airquality{enter}mtcars{enter}{esc}');
    cy.contains('button', 'Load Datasets').click();
    cy.contains('.nav.nav-pills a:not([data-value=teal_data_module])', 'example teal module').click();

    cy.get('.selectize-input').then(($input) => {
      const text = $input.text();
      console.log(text);
      expect(text).to.include('CO2');
      expect(text).to.include('airquality');
      expect(text).to.include('mtcars');
    });
  });
});
