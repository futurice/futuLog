import "app/ui/app/App.scss";

import React from "react";
import { Router, Switch, Route, Redirect } from "react-router-dom";
import { IServices, ServicesContext } from "app/services/services";
import { routes } from "app/utils/routeUtils";
import { BookingPage } from "app/ui/bookingPage/BookingPage";
import { TrackingPage } from "app/ui/trackingPage/TrackingPage";

interface IApp {
  services: IServices;
}

export const App: React.FC<IApp> = ({ services }) => (
  <div className="App">
    <ServicesContext.Provider value={services}>
      {/* TODO: Provide history through services */}
      <Router history={services.history}>
        <Switch>
          <Redirect exact={true} from="/" to={routes.TRACKING} />
          <Route exact={true} path={routes.BOOKING} component={BookingPage} />
          <Route exact={true} path={routes.TRACKING} component={TrackingPage} />
          {/* <Route path="*" component={NotFound} /> */}
        </Switch>
      </Router>
    </ServicesContext.Provider>
  </div>
);
