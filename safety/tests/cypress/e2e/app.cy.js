describe('app', () => {
  beforeEach(() => {
    cy.visit('/');
  });

  it('Starts', () => {});

  it('Has 12 tabs', () => {
    cy
      .get('.nav.nav-pills a[data-bs-toggle=tab]', { timeout: 30000 })
      .should('have.length', 12);
  });

  it('Navigates to all tabs without error', () => {
    cy
      .get('.nav.nav-pills a[data-bs-toggle=tab]', { timeout: 30000 })
      .each(($el) => {
        cy.wrap($el).as('tealTab');

        cy.get('@tealTab').then(($el2) => {
          cy.log(`Navigating to: ${$el2[0].innerText}`);
        });

        cy.get('@tealTab').click();

        cy.get('@tealTab').invoke('attr', 'href').as('hrefTab');

        // Make sure that html element does not have a class that indicates
        // that shiny is busy
        cy.get('html').not('.shiny-busy');

        cy
          .get('@hrefTab')
          .then((hrefTab) => {
            cy
              .get(`${hrefTab}.tab-pane.active`)
              .should('be.visible')
              .within(() => {
                cy
                  .get('*')
                  .filter(':visible')
                  .should('have.length.gte', 1)
                  .then(($el3) => {
                    cy.wrap($el3).contains(/.+/);
                  });
              });
          });
      });
  });
});
