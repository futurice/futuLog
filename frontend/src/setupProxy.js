const { createProxyMiddleware } = require('http-proxy-middleware');

module.exports = function(app) {
  for (const route of ['/api', '/login', '/return', '/swagger-ui', '/swagger.json']) {
    app.use(
      route,
      createProxyMiddleware({
        target: 'http://localhost:8000',
        changeOrigin: true,
      })
    );
  }
};
