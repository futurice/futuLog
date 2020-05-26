import * as React from "react";
import { Route, Switch, Redirect } from "react-router-dom";
import { routes } from "./config";

import { Tracking, Booking } from "../pages";

class Routes extends React.Component {
  public render() {
    return (
      <Switch>
        <Redirect exact={true} from="/" to={routes.TRACKING} />
        <Route exact={true} path={routes.BOOKING} component={Booking} />
        <Route exact={true} path={routes.TRACKING} component={Tracking} />
        {/* <Route path="*" component={NotFound} /> */}
      </Switch>
    );
  }
}

export default Routes;
