import React from "react";
import { Link } from "react-router-dom";
import { Box, Button, Paper, styled } from "@material-ui/core";
import { RoutePaths } from "app/ui/app/AppRoutes";
import { useDispatch } from "app/stores/rootStore";
import {
  useRemoteDataFetch,
  RenderRemoteData,
  useRemoteDataValue,
  pushRemoteData,
} from "app/utils/remoteDataUtils";
import { useServices } from "app/services/services";
import { Workmode, IWorkmodeDto, IUserWorkmodeDto, IUserDto } from "app/services/apiClientService";
import { combineRemoteData, getRemoteDataValue, remoteStore } from "app/stores/remoteStore";
import { WorkmodeButtons } from "app/ui/homePage/WorkmodeButtons";
import { colors } from "app/ui/ux/colors";

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

const Section = styled(Paper)(({ theme }) => ({
  width: "100%",
  maxWidth: "52rem",
  backgroundColor: colors["deep-blue-light"],
  color: colors["deep-blue-dark"],
  [theme.breakpoints.down("sm")]: {
    padding: "2.5rem 1.25rem",
  },
  [theme.breakpoints.up("sm")]: {
    padding: "2.5rem 4rem",
  },
}));

const Separator = styled("hr")({
  // TODO: Rearrange colors into xxx-10, xxx-20, etc..
  // This should then be `deep-blue-20`
  borderColor: "#D2CEE3",
});

export const HomePage: React.FC = () => {
  const { apiClientService } = useServices();
  const date = new Date().toISOString().slice(0, 10);
  const dispatch = useDispatch();

  const user = useRemoteDataValue<IUserDto>("users", "0", null!);
  const userWorkmodeRes = useRemoteDataFetch("userWorkmodesByDay", date, () =>
    apiClientService.getUserWorkmode(date).catch(() => null)
  );
  const userShiftRes = useRemoteDataFetch("userShifts", "0", () => apiClientService.getUserShift());

  const onSelectWorkmode = (workmodeEnum: Workmode) => {
    const userShift = getRemoteDataValue(userShiftRes, null);
    if (userShift) {
      const workmode = hackWorkmode(workmodeEnum);
      const site = userShift.site;
      const userWorkmode = {
        userEmail: user && user.email,
        date,
        site,
        workmode,
      } as IUserWorkmodeDto;
      dispatch([
        // Optimistic prepopulation of the selection, TODO: Reconsider
        remoteStore.actions.setLoaded({
          key: "userWorkmodesByDay",
          id: date,
          value: userWorkmode,
        }),
        pushRemoteData("userWorkmodesByDay", date, () =>
          apiClientService.registerUserWorkmode({ date, site, workmode }).then(() => userWorkmode)
        ),
      ]);
    }
  };

  return (
    <Box
      className="HomePage stack"
      width="100%"
      // maxWidth="40rem"
      display="flex"
      flexDirection="column"
      alignItems="center"
      textAlign="center"
      mx="auto"
      p={["0.5rem", "1rem", "2.5rem"]}
    >
      <RenderRemoteData
        remoteData={combineRemoteData({ userWorkmode: userWorkmodeRes, userShift: userShiftRes })}
        onLoading={(data, children) => children(data || ({} as any), true)}
        onError={(error, children) => children({} as any, false, error)}
      >
        {({ userWorkmode }, isLoading: boolean, error?: Error) => (
          <Section elevation={0} className="stack">
            <h2>Where are you working today?</h2>

            <Box maxWidth="24rem" mx="auto">
              <WorkmodeButtons
                disabled={isLoading}
                workmode={userWorkmode ? userWorkmode.workmode.type : Workmode.Home}
                onSelectWorkmode={onSelectWorkmode}
              />
            </Box>
            {error && <Box component="p">{error.message}</Box>}

            {userWorkmode &&
              userWorkmode.workmode.type === Workmode.Office &&
              !userWorkmode.workmode.confirmed && (
                <>
                  <Separator />
                  <h3>Check in!</h3>

                  <p>
                    You booked a spot to work from the office today. Please confirm that you are in
                    the office. <a href="#">Why?</a>
                  </p>

                  <Button>I'm in the office</Button>
                </>
              )}
          </Section>
        )}
      </RenderRemoteData>

      <Section elevation={0} className="stack">
        <h2>Where will you work work in the next two weeks?</h2>

        <p>
          Check your options based on your shift and plan where you will be working in the future.
        </p>

        <Link to={RoutePaths.Planning}>Planning</Link>
      </Section>
    </Box>
  );
};
