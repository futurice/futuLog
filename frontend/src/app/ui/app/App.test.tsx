import React from "react";
import { render } from "@testing-library/react";
import { App } from "./App";
import { createServices } from "app/services/services";

test("renders learn react link", () => {
  const services = createServices();
  const res = render(<App services={services} />);
  const el = res.getByText(/Tracking/i);
  expect(el).toBeInTheDocument();
});
