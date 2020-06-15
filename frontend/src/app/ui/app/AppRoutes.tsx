import React from "react";
import { Switch, Route } from "react-router-dom";
import { HomePage } from "app/ui/homePage/HomePage";
import { useRemoteDataFetch, RenderRemoteData } from "app/utils/remoteDataUtils";
import { useServices } from "app/services/services";

export enum RoutePaths {
  Home = "/",
  Welcome = "/welcome",
  Planning = "/planning",
}

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
            <Route exact path={RoutePaths.Home} component={HomePage} />
            {/* <Route path="*" component={NotFound} /> */}
          </Switch>
        </>
      )}
    </RenderRemoteData>
  );
};
