import React, { useEffect, useState } from "react";
import { Link } from "react-router-dom";
import { RoutePaths } from "app/ui/app/AppRoutes";
import { useDispatch, useSelector } from "app/stores/rootStore";
import { Workmode, IWorkmode } from "app/stores/workmodeStore";
import { fetchUserWorkmode, pushUserWorkmode } from "app/stores/userStore";
import {
  useRemoteDataFetch,
  RenderRemoteData,
} from "app/utils/remoteDataUtils";
import { useServices } from "app/services/services";
import { IAPIClientService } from "app/services/apiClientService";
import { RemoteData } from "app/stores/remoteStore";

const workmodes = [
  Workmode.Home,
  Workmode.Office,
  Workmode.Client,
  Workmode.Leave,
];

function hackWorkmode(workmodeStr: string): IWorkmode {
  switch (workmodeStr) {
    case Workmode.Home:
      return { type: Workmode.Home };
    case Workmode.Office:
      return { type: Workmode.Office, confirmed: false };
    case Workmode.Client:
      return { type: Workmode.Client, name: "<Unknown>" };
    case Workmode.Leave:
      return { type: Workmode.Leave };
  }

  return { type: Workmode.Home };
}

async function hackFetchWorkmode(
  userEmail: string,
  date: string,
  apiClientService: IAPIClientService
) {
  await new Promise((resolve) => setTimeout(resolve, 2000));
  //
  // TODO: This is a temporary hack to get the current workmode for the user
  // using the development endpoint that gives all registered workmodes.
  // Actual implementation should use an endpoint that delivers the current
  // workmode for the authorized user, which doesn't exist yet

  const workmodes = await apiClientService.getWorkmodes();
  const workmode = workmodes.find(
    (workmode) => workmode.date === date && workmode.userEmail === userEmail
  );

  if (!workmode) {
    throw Error("Not found");
  }

  return workmode;
}

export const EntrancePage: React.FC = () => {
  const { apiClientService } = useServices();
  const [testDate, setTestDate] = useState(
    new Date().toISOString().slice(0, 10)
  );
  const user = useSelector((state) => state.userStore.user);
  const dispatch = useDispatch();

  const onSelectWorkmode = (workmodeStr: string) => {
    dispatch(pushUserWorkmode(testDate, hackWorkmode(workmodeStr)));
  };

  const workmodeRes = useRemoteDataFetch(
    "userWorkmodesByDay",
    user && testDate,
    () => hackFetchWorkmode(user!.email, testDate, apiClientService),
    [user]
  );

  useEffect(() => {
    dispatch(fetchUserWorkmode(testDate));
  }, [dispatch, testDate]);

  return (
    <div className="EntrancePage">
      <section>
        <h2>Where are you working today?</h2>

        <label>
          <span>Selected option</span>
          <select onChange={(ev) => onSelectWorkmode(ev.currentTarget.value)}>
            {workmodes.map((workmode) => (
              <option key={workmode} value={workmode}>
                {workmode}
              </option>
            ))}
          </select>
        </label>

        <label>
          <span>(TEST) Date: </span>
          <input
            type="text"
            value={testDate}
            onChange={(ev) => setTestDate(ev.currentTarget.value)}
          />
        </label>
      </section>

      <section>
        <h2>Workmode</h2>
        <RenderRemoteData
          remoteData={workmodeRes}
          onLoading={() => <h3>Loading...</h3>}
          onError={(error) => (
            <h3 style={{ color: "red" }}>Error: {error.message}</h3>
          )}
          onLoaded={(workmode) => (
            <pre>{JSON.stringify(workmode, null, 2)}</pre>
          )}
        />
      </section>

      <section>
        <h2>Where will you work work in the next two weeks?</h2>

        <p>
          Check your options based on your shift and plan where you will be
          working in the future.
        </p>

        <Link to={RoutePaths.Planning}>Planning</Link>
      </section>
    </div>
  );
};
