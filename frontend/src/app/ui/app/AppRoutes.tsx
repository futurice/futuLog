import React from "react";
import { Switch, Route } from "react-router-dom";
import { BookingPage } from "app/ui/bookingPage/BookingPage";
import { TrackingPage } from "app/ui/trackingPage/TrackingPage";
import { EntrancePage } from "app/ui/entrancePage/EntrancePage";
import { PlanningPage } from "app/ui/planningPage/PlanningPage";
import { useRemoteDataFetch, RenderRemoteData } from "app/utils/remoteDataUtils";
import { useServices } from "app/services/services";

export enum RoutePaths {
  Entrance = "/",
  Booking = "/booking",
  Planning = "/planning",
  Tracking = "/tracking",
}
/*
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
        <Route path="*" component={NotFound} />
      </Switch>
    </>
  );
};
*/
export const AppRoutes: React.FC = () => {
  const { apiClientService } = useServices();

  // Fetch all critical data here
  const userRes = useRemoteDataFetch("users", "0", () => apiClientService.getUser());

  return (
    <RenderRemoteData
      remoteData={userRes}
      onLoading={() => <h2>Loading user information..</h2>}
      onError={(error) => <h2>{error.message}</h2>}
    >
      {(_user) => (
        <>
          <Switch>
            <Route exact path={RoutePaths.Entrance} component={EntrancePage} />
            <Route exact path={RoutePaths.Booking} component={BookingPage} />
            <Route exact path={RoutePaths.Planning} component={PlanningPage} />
            <Route exact path={RoutePaths.Tracking} component={TrackingPage} />
            {/* <Route path="*" component={NotFound} /> */}
          </Switch>
        </>
      )}
    </RenderRemoteData>
  );
};
