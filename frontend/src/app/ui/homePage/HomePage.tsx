import React, { useState } from "react";
import { Box, styled, IconButton } from "@material-ui/core";
import { useQuery, useMutation } from "react-query";
import { useServices } from "app/services/services";
import {
  Workmode,
  IWorkmodeDto,
  IShiftAssignmentDto,
  IOfficeSpaceDto,
  IRegisterWorkmodeDto,
  ICapacityDto
} from "app/services/apiClientService";
import {
  RenderQuery,
  combineQueries,
  officeBookingsQueryKey,
  userWorkmodeQueryKey,
  userShiftQueryKey,
  officesQueryKey,
} from "app/utils/reactQueryUtils";
import { RoutePaths } from "app/ui/app/AppRoutes";
import { colors } from "app/ui/ux/theme";
import { Button, LinkButton } from "app/ui/ux/buttons";
import { H2, H3, P } from "app/ui/ux/text";
import { IconInfo, IconCheck } from "app/ui/ux/icons";
import { Stack, HR } from "app/ui/ux/containers";
import { WorkmodeButtons } from "app/ui/homePage/WorkmodeButtons";

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

const getOfficeCapacity = (
  office: IOfficeSpaceDto,
  date: string,
  officeBookings: ICapacityDto[]
) => {
  const booking = officeBookings.find((booking) => booking.date === date);
  return booking ? office.maxPeople - booking.people.length : office.maxPeople;
};

export const HomePage: React.FC = () => {
  const { apiClient, queryCache } = useServices();
  const date = new Date().toISOString().slice(0, 10);
  const [isWhyExpanded, setIsWhyExpanded] = useState(false);

  //
  // Remote data

  // These are pre-loaded in AppRoutes
  const userShift = queryCache.getQueryData<IShiftAssignmentDto>(userShiftQueryKey());
  const offices = queryCache.getQueryData<IOfficeSpaceDto[]>(officesQueryKey());

  const userWorkmodeRes = useQuery(userWorkmodeQueryKey(date), () =>
    apiClient.getUserWorkmode(date).catch(() => null)
  );
  const officeBookingsRes = useQuery(
    userShift && officeBookingsQueryKey(userShift.site, date, date),
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    () => apiClient.getOfficeBookings({ site: userShift!.site, startDate: date, endDate: date })
  );



  const [registerWorkmode] = useMutation(
    (request: IRegisterWorkmodeDto) => apiClient.registerUserWorkmode([request]),
    {
      onSuccess: () => queryCache.refetchQueries(userWorkmodeQueryKey(date)),
    }
  );
  const [confirmWorkmode] = useMutation(() => apiClient.confirmUserWorkmode(true), {
    onSuccess: () => queryCache.refetchQueries(userWorkmodeQueryKey(date)),
  });

  const userOffice = (offices || []).find((office) => userShift && office.site === userShift.site);

  //
  // Actions

  const onSelectWorkmode = (workmode: IWorkmodeDto) => {
    if (userShift) {
      const site = userShift.site;
      registerWorkmode({ date, site, workmode });
    }
  };

  const onConfirmOffice = () => {
    confirmWorkmode();
  };

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
      <RenderQuery
        query={combineQueries({
          userWorkmode: userWorkmodeRes,
          officeBookings: officeBookingsRes,
        })}
        // TODO: Improve the UX for loading state, don't let the initial state flash
        onLoading={(data, children) => children(data || ({} as any), true)}
        onError={(error, children) => children({} as any, false, error)}
      >
        {({ userWorkmode, officeBookings }, isLoading: boolean, error?: Error) => (
          <Card spacing="2rem" textAlign="center">
            <Stack spacing="2rem" maxWidth="26rem" mx="auto">
              <H2>Where are you working today?</H2>

              <Box maxWidth="24rem" mx="auto">
                <WorkmodeButtons
                  disabled={isLoading}
                  officeCapacity={
                    userOffice ? getOfficeCapacity(userOffice, date, officeBookings || []) : 0
                  }
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
                        mx="auto"
                        fontSize="1.5rem"
                        fontWeight="bold"
                        fontFamily="Futurice"
                        lineHeight="1.75"
                        marginBottom="0"
                      >
                      <IconCheck />
                        <br />
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
      </RenderQuery>

      <Card spacing="2rem" textAlign="center">
        <Stack spacing="1.25rem" maxWidth="26rem" mx="auto">
          <H2>Where will you work in the next two weeks?</H2>

          <P>
            Plan in advance where you want to work in the next weeks.
          </P>

          <LinkButton to={RoutePaths.Planning} variant="contained" color="primary">
            Planning
          </LinkButton>
        </Stack>
      </Card>
    </Stack>
  );
};
