import React from "react";
import { Switch, Route, Redirect } from "react-router-dom";
import { BookingPage } from "app/ui/bookingPage/BookingPage";
import { TrackingPage } from "app/ui/trackingPage/TrackingPage";
import { EntrancePage } from "app/ui/entrancePage/EntrancePage";
import { PlanningPage } from "app/ui/planningPage/PlanningPage";
import { FakeLoginPage } from "app/ui/fakeLoginPage/FakeLoginPage";
import { useRemoteDataValue } from "app/utils/remoteDataUtils";

export enum RoutePaths {
  Entrance = "/",
  FakeLogin = "/login",
  Booking = "/booking",
  Planning = "/planning",
  Tracking = "/tracking",
}

export const AppRoutes: React.FC = () => {
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const user = useRemoteDataValue("users", "0", null!);
  console.log("user?", user);

  return (
    <>
      {!user && <Redirect exact to={RoutePaths.FakeLogin} />}

      <Switch>
        <Route exact path={RoutePaths.Entrance} component={EntrancePage} />
        <Route exact path={RoutePaths.FakeLogin} component={FakeLoginPage} />
        <Route exact path={RoutePaths.Booking} component={BookingPage} />
        <Route exact path={RoutePaths.Planning} component={PlanningPage} />
        <Route exact path={RoutePaths.Tracking} component={TrackingPage} />
        {/* <Route path="*" component={NotFound} /> */}
      </Switch>
    </>
  );
};
