/*
  Small proxy server to handle CORS issues in development
*/
const http = require("http");
const httpProxy = require("http-proxy");

const corsHeaders = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Headers": "*",
  "Access-Control-Allow-Methods": "*",
};

var proxy = httpProxy.createProxyServer({
  target: "http://localhost:8000",
  headers: corsHeaders,
});

proxy.on("proxyRes", (proxyRes, req) => {
  console.log(`< ${proxyRes.statusCode} ${req.method} ${req.url}`);
});

const server = http.createServer(function (req, res) {
  console.log(`> ${req.method} ${req.url}`);

  if (req.method === "OPTIONS") {
    res.writeHead(204, corsHeaders);
    return res.end();
  }

  proxy.web(req, res);
});

server.listen(5000, () => {
  const addr = server.address();
  console.log(
    `Proxy listening at ${typeof addr === "string" ? addr : `${addr.address}:${addr.port}`}`
  );
});
