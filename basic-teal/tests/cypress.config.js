const { defineConfig } = require("cypress");

module.exports = defineConfig({
  e2e: {
    setupNodeEvents(on, config) {},
    allowCypressEnv: false,
    baseUrl: "http://localhost:3333",
    supportFile: "cypress/support/commands.js",
  },
});
