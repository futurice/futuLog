import React, { useState } from "react";
import { Box, styled, IconButton } from "@material-ui/core";
import { useDispatch } from "app/stores/rootStore";
import {
  useRemoteDataFetch,
  RenderRemoteData,
  useRemoteDataValue,
  pushRemoteData,
} from "app/utils/remoteDataUtils";
import { useServices } from "app/services/services";
import {
  Workmode,
  IWorkmodeDto,
  IUserWorkmodeDto,
  IUserDto,
  IShiftAssignmentDto,
  IOfficeSpaceDto,
} from "app/services/apiClientService";
import { combineRemoteData, remoteStore } from "app/stores/remoteStore";
import { WorkmodeButtons } from "app/ui/homePage/WorkmodeButtons";
import { colors } from "app/ui/ux/theme";
import { Button } from "app/ui/ux/buttons";
import { H2, H3, P } from "app/ui/ux/text";
import { IconInfo } from "app/ui/ux/icons";
import { Stack, HR } from "app/ui/ux/containers";

const Card = styled(Stack)(({ theme }) => ({
  width: "100%",
  maxWidth: "52rem",
  backgroundColor: colors["deep-blue-10"],
  color: colors["deep-blue-80"],
  [theme.breakpoints.down("sm")]: {
    padding: "2.5rem 1.25rem",
  },
  [theme.breakpoints.up("md")]: {
    padding: "2.5rem 4rem",
  },
}));

const InlineIconButton = styled(IconButton)({
  padding: "0.5rem",
  fontSize: "inherit",
  "& > .MuiIconButton-label": {
    display: "inline",
  },
});

export const HomePage: React.FC = () => {
  const { apiClientService } = useServices();
  const date = new Date().toISOString().slice(0, 10);
  const dispatch = useDispatch();
  const [isWhyExpanded, setIsWhyExpanded] = useState(false);

  //
  // Remote data

  // These are pre-loaded in AppRoutes
  /* eslint-disable @typescript-eslint/no-non-null-assertion */
  const user = useRemoteDataValue<IUserDto>("users", "0", null!);
  const userShift = useRemoteDataValue<IShiftAssignmentDto>("userShifts", "0", null!);
  const offices = useRemoteDataValue<IOfficeSpaceDto[]>("offices", "0", null!);
  /* eslint-disable @typescript-eslint/no-non-null-assertion */

  const userWorkmodeRes = useRemoteDataFetch("userWorkmodesByDay", date, () =>
    apiClientService.getUserWorkmode(date).catch(() => null)
  );
  const officeCapacityRes = useRemoteDataFetch(
    "officeCapacityBySiteDate",
    userShift && `${userShift.site}/${date}`,
    () => apiClientService.getOfficeCapacity(userShift.site, date)
  );

  const userOffice = (offices || []).find((office) => userShift && office.site === userShift.site);

  //
  // Actions

  const onSelectWorkmode = (workmode: IWorkmodeDto) => {
    if (userShift) {
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

  const onConfirmOffice = () =>
    dispatch(
      pushRemoteData("userWorkmodesByDay", date, () =>
        apiClientService
          .confirmUserWorkmode(true)
          .then(() => apiClientService.getUserWorkmode(date).catch(() => null))
      )
    );

  //
  // View

  return (
    <Stack
      className="HomePage"
      display="flex"
      flexDirection="column"
      alignItems="center"
      // textAlign="center"
      mx="auto"
      p={["0.5rem", "1rem", "2.5rem"]}
      spacing={["0.5rem", "1rem", "2.5rem"]}
    >
      <RenderRemoteData
        remoteData={combineRemoteData({
          userWorkmode: userWorkmodeRes,
          officeCapacity: officeCapacityRes,
        })}
        // TODO: Improve the UX for loading state, don't let the initial state flash
        onLoading={(data, children) => children(data || ({} as any), true)}
        onError={(error, children) => children({} as any, false, error)}
      >
        {({ userWorkmode, officeCapacity }, isLoading: boolean, error?: Error) => (
          <Card spacing="2rem" textAlign="center">
            <Stack spacing="2rem" maxWidth="26rem" mx="auto">
              <H2>Where are you working today?</H2>

              <Box maxWidth="24rem" mx="auto">
                <WorkmodeButtons
                  disabled={isLoading}
                  officeCapacity={(userOffice ? userOffice.maxPeople : 0) - (officeCapacity || 0)}
                  workmode={userWorkmode ? userWorkmode.workmode : { type: Workmode.Home }}
                  onSelectWorkmode={onSelectWorkmode}
                />
              </Box>
              {error && <Box component="p">{error.message}</Box>}
            </Stack>

            {/* Office check-in status */}
            {userWorkmode && userWorkmode.workmode.type === Workmode.Office && (
              <>
                <HR />
                {!userWorkmode.workmode.confirmed ? (
                  //
                  // Not checked in yet
                  <Stack spacing="1.25rem" maxWidth="26rem" mx="auto">
                    <H3>Check in!</H3>

                    <P>
                      You have booked a spot to work from the office today. Please confirm that you
                      are there or that you are going and that you feel healthy.
                      <InlineIconButton
                        aria-label="More information"
                        aria-expanded={isWhyExpanded}
                        disableRipple
                        onClick={() => setIsWhyExpanded(!isWhyExpanded)}
                      >
                        <IconInfo />
                      </InlineIconButton>
                    </P>

                    {isWhyExpanded && (
                      <>
                        <P>
                          Since Futurice needs to track who went to the office, we need to be sure
                          who went there and we need to be sure about you feeling healthy and do not
                          have any of these symptoms: dry cough, sore throat, fever or general
                          feeling of sickness.
                        </P>
                      </>
                    )}

                    <Button variant="contained" color="primary" onClick={onConfirmOffice}>
                      I'm in the office
                    </Button>
                  </Stack>
                ) : (
                  //
                  // Checked in
                  <>
                    <Box
                      component={P}
                      maxWidth="26rem"
                      fontSize="1.5rem"
                      fontWeight="bold"
                      fontFamily="Futurice"
                      lineHeight="1.75"
                      marginBottom="0"
                    >
                      ✓<br />
                      You are checked in!
                      <br />
                      Thank you.
                    </Box>
                  </>
                )}
              </>
            )}
          </Card>
        )}
      </RenderRemoteData>

      {/*
      <Card spacing="2rem" textAlign="center">
        <Stack spacing="1.25rem" maxWidth="26rem" mx="auto">
          <H2>Where will you work work in the next two weeks?</H2>

          <P>
            Check your options based on your shift and plan where you will be working in the future.
          </P>

          <LinkButton to={RoutePaths.Planning} variant="contained" color="primary">
            Planning
          </LinkButton>
        </Stack>
      </Card>
      */}
    </Stack>
  );
};
