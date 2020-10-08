import React, { useState } from "react";
import { RoutePaths } from "app/ui/app/AppRoutes";
import { Box, styled } from "@material-ui/core";
import { useQuery, useMutation } from "react-query";
import { useServices } from "app/services/services";
import {
  Workmode,
  IShiftAssignmentDto,
  ISetShiftDto,
} from "app/services/apiClientService";
import {
  RenderQuery,
  combineQueries,
  officeBookingsQueryKey,
  userWorkmodeQueryKey,
  userShiftQueryKey,
} from "app/utils/reactQueryUtils";
import { colors } from "app/ui/ux/theme";
import { LinkButton } from "app/ui/ux/buttons";
import { H2, H3, P } from "app/ui/ux/text";
import { IconCheck, IconOffice} from "app/ui/ux/icons";
import { Stack, HR } from "app/ui/ux/containers";
import { WorkmodeButton } from "app/ui/homePage/WorkmodeButtons";
import { Card } from "app/ui/ux/Card"


export const QrHelsinki: React.FC = () => {
  const { apiClient, queryCache } = useServices();
  const date = new Date().toISOString().slice(0, 10);
  const [ userConfirmed, setUserConfirmed ] = useState(
    false
  );


  const [registerSiteShift] = useMutation(
    (request: ISetShiftDto) => apiClient.registerSiteShift(request),
    {
      onSuccess: () => {
        queryCache.refetchQueries(userShiftQueryKey());
      },
    }
  );

  const [registerUserWorkmode] = useMutation(
    () => apiClient.registerUserWorkmode([{ site: "Helsinki", date: date,
           workmode:{ type: Workmode.Office, confirmed:true} }]),
    {
      onSuccess: () => queryCache.refetchQueries(userWorkmodeQueryKey(date)),
    }
  );

  const [confirmUserWork] = useMutation(
    () => apiClient.confirmUserWorkmode(true),
    {
      onSuccess: () => queryCache.refetchQueries(userWorkmodeQueryKey(date)),
    }
  );


  const confirmUserOffice = () => {
    registerSiteShift({ shiftName: "default", site: "Helsinki" })
    registerUserWorkmode()
    confirmUserWork()
    setUserConfirmed(true)
  }

  const userShift = queryCache.getQueryData<IShiftAssignmentDto>(userShiftQueryKey());

  const userWorkmodeRes = useQuery(userWorkmodeQueryKey(date), () =>
    apiClient.getUserWorkmode(date).catch(() => null)
  );
  const officeBookingsRes = useQuery(
    userShift && officeBookingsQueryKey(userShift.site, date, date),

    () => apiClient.getOfficeBookings({ site: userShift!.site, startDate: date, endDate: date })
  );

  if (!userConfirmed) {
    confirmUserOffice()
  }


  return(
    <Stack spacing="2.5rem" maxWidth="25rem" mx="auto" textAlign="center">

    <RenderQuery
      query={combineQueries({
        userWorkmode: userWorkmodeRes,
        officeBookings: officeBookingsRes,
      })}
      onLoading={(data, children) => children(data || ({} as any), true)}
      onError={(error, children) => children({} as any, false, error)}
    >

      {({ userWorkmode, officeBookings }, isLoading: boolean, error?: Error) =>
      <Stack spacing="2.5rem" maxWidth="25rem" mx="auto" textAlign="center">
        <Card spacing="2rem" textAlign="center">

          <H2>Where are you working today?</H2>
          <WorkmodeButton
            disabled={false}
            active={true}
            startIcon={<IconOffice />}
            hoverColor="jade-green-10"
            onClick={()=>{}}
          >
          Office
          </WorkmodeButton>
          <HR/>
          <Box
            component={H3}
            maxWidth="26rem"
            mx="auto"
            fontSize="1rem"
            fontWeight="bold"
            fontFamily="Futurice"
            lineHeight="1.75"
            marginBottom="0"
          >
            <IconCheck/>
            <br />

          You are checked in!
          <br />

          Thank you.
        </Box>
        </Card>
      <br />

      <Card spacing="2rem" textAlign="center">
        <Stack spacing="1.25rem" maxWidth="26rem" mx="auto">
          <H2>Where will you work in the next two weeks?</H2>
          <P>
            Plan in advance where you want to work in the next weeks.
          </P>
          <LinkButton to={RoutePaths.Planning} variant="contained" color="primary">
            Plan
          </LinkButton>
          </Stack>
      </Card>

      </Stack>

    }

    </RenderQuery>

    </Stack>

)

}
