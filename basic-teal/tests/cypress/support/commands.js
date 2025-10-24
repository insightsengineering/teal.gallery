Cypress.on("uncaught:exception", (err, runnable) => {
  if (
    err.message.includes(
      "ResizeObserver loop completed with undelivered notifications"
    ) ||
    err.message.includes("ResizeObserver loop limit exceeded") ||
    err.message.includes(
      "Cannot read properties of undefined (reading 'onResize')"
    )
  ) {
    return false;
  }
  return true;
});

Cypress.Commands.add("waitForShinyStabilityAndCheckError", () => {
  const idleFor = 2000; // ms of stable (no .shiny-busy)
  const checkInterval = 200; // ms, less frequent to avoid ResizeObserver spam
  const timeout = 10000; // ms
  let stableStart = null;
  const start = Date.now();

  function check() {
    cy.get("html").then(($el) => {
      const isBusy = $el.hasClass("shiny-busy");
      const now = Date.now();

      if (!isBusy) {
        if (stableStart === null) stableStart = now;
        if (now - stableStart >= idleFor) {
          cy.wait(300).then(() => {
            cy.get("body").then(($body) => {
              const error = $body.find(
                ".shiny-output-error:not(.shiny-output-error-validation)"
              );
              if (error.length > 0) {
                throw new Error(
                  "Detected .shiny-output-error without .shiny-output-error-validation!"
                );
              }
            });
          });
          return;
        }
      } else {
        stableStart = null;
      }

      if (now - start > timeout) {
        throw new Error("Timed out waiting for Shiny to become idle");
      }
      cy.wait(checkInterval).then(check);
    });
  }

  check();
});
