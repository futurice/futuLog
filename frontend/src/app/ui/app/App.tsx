import "app/ui/app/App.scss";

import React from "react";
import { Router, Switch, Route, Redirect } from "react-router-dom";
import { history, routes } from "app/utils/routeUtils";
import { BookingPage } from "app/ui/bookingPage/BookingPage";
import { TrackingPage } from "app/ui/trackingPage/TrackingPage";

export const App: React.FC = () => (
  <div className="App">
    {/* TODO: Provide history through services */}
    <Router history={history}>
      <Switch>
        <Redirect exact={true} from="/" to={routes.TRACKING} />
        <Route exact={true} path={routes.BOOKING} component={BookingPage} />
        <Route exact={true} path={routes.TRACKING} component={TrackingPage} />
        {/* <Route path="*" component={NotFound} /> */}
      </Switch>
    </Router>
  </div>
);
