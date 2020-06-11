import React from "react";
import { Link } from "react-router-dom";
import { RoutePaths } from "app/ui/app/AppRoutes";
import { useDispatch } from "app/stores/rootStore";
import {
  useRemoteDataFetch,
  RenderRemoteData,
  useRemoteDataValue,
} from "app/utils/remoteDataUtils";
import { useServices } from "app/services/services";
import { Workmode, IWorkmodeDto, IUserWorkmodeDto, IUserDto } from "app/services/apiClientService";
import { combineRemoteData, getRemoteDataValue, remoteStore } from "app/stores/remoteStore";
import { WorkmodeButtons } from "app/ui/entrancePage/WorkmodeButtons";

const workmodes = [Workmode.Home, Workmode.Office, Workmode.Client, Workmode.Leave];

function hackWorkmode(workmodeStr: string): IWorkmodeDto {
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
  const { apiClientService } = useServices();
  const date = new Date().toISOString().slice(0, 10);
  const dispatch = useDispatch();

  const user = useRemoteDataValue<IUserDto>("users", "0", null!);
  const userWorkmodeRes = useRemoteDataFetch("userWorkmodesByDay", date, () =>
    apiClientService.getUserWorkmode(date).catch(() => null)
  );
  const userShiftRes = useRemoteDataFetch("userShifts", "0", () => apiClientService.getUserShift());

  const onSelectWorkmode = async (workmodeEnum: Workmode) => {
    const userShift = getRemoteDataValue(userShiftRes, null);
    if (userShift) {
      const workmode = hackWorkmode(workmodeEnum);
      const site = userShift.site;
      dispatch(
        remoteStore.actions.setLoaded({
          key: "userWorkmodesByDay",
          id: date,
          value: {
            userEmail: user && user.email,
            date,
            site,
            workmode,
          } as IUserWorkmodeDto,
        })
      );
      await apiClientService.registerUserWorkmode({
        date,
        site,
        workmode,
      });
    }
  };

  return (
    <div className="EntrancePage">
      <RenderRemoteData
        remoteData={combineRemoteData({ userWorkmode: userWorkmodeRes, userShift: userShiftRes })}
        onLoading={() => <h2>Loading user information..</h2>}
      >
        {({ userWorkmode }) => (
          <>
            <h2>Where are you working today?</h2>

            <WorkmodeButtons
              workmode={userWorkmode ? userWorkmode.workmode.type : Workmode.Home}
              onSelectWorkmode={onSelectWorkmode}
            />

            <label>
              <span>Selected option</span>
              <select
                onChange={(ev) => onSelectWorkmode(ev.currentTarget.value as Workmode)}
                value={userWorkmode ? userWorkmode.workmode.type : Workmode.Home}
              >
                {workmodes.map((workmode) => (
                  <option key={workmode} value={workmode}>
                    {workmode}
                  </option>
                ))}
              </select>
            </label>
          </>
        )}
      </RenderRemoteData>

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
