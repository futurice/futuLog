import React, { useEffect, useState } from "react";
import { Link } from "react-router-dom";
import { RoutePaths } from "app/ui/app/AppRoutes";
import { useDispatch } from "app/stores/rootStore";
import { Workmode, IWorkmode } from "app/stores/workmodeStore";
import { fetchUserWorkmode, pushUserWorkmode } from "app/stores/userStore";

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

export const EntrancePage: React.FC = () => {
  const [testDate, setTestDate] = useState(
    new Date().toISOString().slice(0, 10)
  );
  const dispatch = useDispatch();

  const onSelectWorkmode = (workmodeStr: string) => {
    dispatch(pushUserWorkmode(testDate, hackWorkmode(workmodeStr)));
  };

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
