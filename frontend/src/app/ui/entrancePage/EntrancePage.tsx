import React, { useState } from "react";
import { Link } from "react-router-dom";
import { RoutePaths } from "app/ui/app/AppRoutes";
import { useDispatch } from "app/stores/rootStore";
import { Workmode, IWorkmode } from "app/stores/workmodeStore";
import { userStore, IUser } from "app/stores/userStore";
import {
  useRemoteDataFetch,
  RenderRemoteData,
  useRemoteDataValue,
} from "app/utils/remoteDataUtils";
import { useServices } from "app/services/services";

const workmodes = [Workmode.Home, Workmode.Office, Workmode.Client, Workmode.Leave];

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

export const EntrancePage: React.FC = () => {
  const services = useServices();
  const [testDate, setTestDate] = useState(new Date().toISOString().slice(0, 10));
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const user = useRemoteDataValue<IUser>("users", "0", null!);
  const dispatch = useDispatch();

  const onSelectWorkmode = (workmodeStr: string) => {
    dispatch(userStore.actions.pushUserWorkmode(testDate, hackWorkmode(workmodeStr)));
  };

  const workmodeRes = useRemoteDataFetch(
    "userWorkmodesByDay",
    user && testDate,
    userStore.fetchers.fetchUserWorkmode(services),
    [user]
  );

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
          onError={(error) => <h3 style={{ color: "red" }}>Error: {error.message}</h3>}
          onLoaded={(workmode) => <pre>{JSON.stringify(workmode, null, 2)}</pre>}
        />
      </section>

      <section>
        <h2>Where will you work work in the next two weeks?</h2>

        <p>
          Check your options based on your shift and plan where you will be working in the future.
        </p>

        <Link to={RoutePaths.Planning}>Planning</Link>
      </section>
    </div>
  );
};
