import React, { useState } from "react";
import { Link } from "react-router-dom";
import { Box, Paper, styled } from "@material-ui/core";
import { RoutePaths } from "app/ui/app/AppRoutes";
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
import { FauxLink, Button } from "app/ui/ux/buttons";
import { H2, H3, P } from "app/ui/ux/text";

const Section = styled(Paper)(({ theme }) => ({
  width: "100%",
  maxWidth: "52rem",
  backgroundColor: colors["deep-blue-10"],
  color: colors["deep-blue-60"],
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
  const [isWhyExpanded, setIsWhyExpanded] = useState(false);

  //
  // Remote data

  // These are pre-loaded in AppRoutes
  const user = useRemoteDataValue<IUserDto>("users", "0", null!);
  const userShift = useRemoteDataValue<IShiftAssignmentDto>("userShifts", "0", null!);
  const offices = useRemoteDataValue<IOfficeSpaceDto[]>("offices", "0", null!);

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
        remoteData={combineRemoteData({
          userWorkmode: userWorkmodeRes,
          officeCapacity: officeCapacityRes,
        })}
        // TODO: Improve the UX for loading state, don't let the initial state flash
        onLoading={(data, children) => children(data || ({} as any), true)}
        onError={(error, children) => children({} as any, false, error)}
      >
        {({ userWorkmode, officeCapacity }, isLoading: boolean, error?: Error) => (
          <Section elevation={0} className="stack">
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

            {userWorkmode && userWorkmode.workmode.type === Workmode.Office && (
              <>
                <Separator />
                {!userWorkmode.workmode.confirmed ? (
                  <section>
                    <H3>Check in!</H3>

                    <P>
                      You booked a spot to work from the office today. Please confirm that you are
                      in the office.{" "}
                      <FauxLink
                        aria-expanded={isWhyExpanded}
                        onClick={() => setIsWhyExpanded(!isWhyExpanded)}
                      >
                        Why?
                      </FauxLink>
                    </P>

                    {isWhyExpanded && (
                      <>
                        <P>
                          Since available spots are limited, if you are not going maybe somebody
                          else could use the spot.
                        </P>
                        <P>
                          We need to keep track of who is in the office to be able to contain an
                          eventual virus spread.
                        </P>
                      </>
                    )}

                    <Button variant="contained" color="primary" onClick={onConfirmOffice}>
                      I'm in the office
                    </Button>
                  </section>
                ) : (
                  <>
                    <Box
                      component={P}
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
          </Section>
        )}
      </RenderRemoteData>

      <Section elevation={0} className="stack">
        <H2>Where will you work work in the next two weeks?</H2>

        <P>
          Check your options based on your shift and plan where you will be working in the future.
        </P>

        <Link to={RoutePaths.Planning}>
          <Button variant="contained" color="primary">
            Planning
          </Button>
        </Link>
      </Section>
    </Box>
  );
};
