import React, { useState } from "react";
import dayjs from "dayjs";
import { styled, useMediaQuery, Box, IconButton } from "@material-ui/core";
import { DatePicker } from "@material-ui/pickers";
import { H4, H2Center } from "app/ui/ux/text";
import { Flex, Stack } from "app/ui/ux/containers";
import { Theme } from "app/ui/ux/theme";
import { IconClose } from "app/ui/ux/icons";
import { WorkmodeButtons } from "app/ui/homePage/WorkmodeButtons";
import {
  Workmode,
  IWorkmodeDto,
  IShiftAssignmentDto,
  IOfficeSpaceDto,
} from "app/services/apiClientService";
import { useServices } from "app/services/services";
import {
  userShiftQueryKey,
  officesQueryKey,
  officeBookingsQueryKey,
} from "app/utils/reactQueryUtils";
import { useQuery } from "react-query";
import { Button } from "app/ui/ux/buttons";

interface IPlanningCalendarDay {
  date: dayjs.Dayjs;
  onClose: () => void;
}

const Root = styled("div")({
  position: "relative",
  display: "flex",
  flexDirection: "column",
  width: "100%",
  // Primary font for the widget, few places use Roboto
  fontFamily: "Futurice",
});

const CloseButtonContainer = styled("div")({
  position: "absolute",
  right: 0,
  top: 0,
});

const WidgetContainer = styled((props) => (
  <Stack {...props} spacing={["1.5rem", "1.5rem", "2rem"]} />
))({
  maxWidth: "24rem",
  margin: "0 auto",
  padding: "1rem 0",
});

const DatePickerContainer = styled("div")({
  width: "50%",
  padding: "0.5rem",
});

const OfficeInfoContainer = styled("p")({
  fontFamily: "Roboto",
  fontSize: "1rem",
  lineHeight: 1.5,
});

export const PlanningCalendarDay: React.FC<IPlanningCalendarDay> = ({ date, onClose }) => {
  const { apiClient, queryCache } = useServices();
  const [startDate, setStartDate] = useState(date);
  const [endDate, setEndDate] = useState(date);
  const [workmode, setWorkmode] = useState<IWorkmodeDto>({ type: Workmode.Home });
  const isMobile = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));
  const startDateStr = startDate.format("YYYY-MM-DD");
  const endDateStr = endDate.format("YYYY-MM-DD");

  // These should be pre-loaded by AppRoutes
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const userShift = queryCache.getQueryData<IShiftAssignmentDto>(userShiftQueryKey())!;
  const offices = queryCache.getQueryData<IOfficeSpaceDto[]>(officesQueryKey());

  const userOffice = (offices || []).find((office) => userShift && office.site === userShift.site);

  const officeBookingsRes = useQuery(
    userOffice && officeBookingsQueryKey(userOffice.site, startDateStr, endDateStr),
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    () => apiClient.getOfficeBookings(userOffice!.site, startDateStr, endDateStr)
  );

  console.log({ userShift, offices, userOffice, officeBookingsRes });

  const onSelectDateRange = (startDate: dayjs.Dayjs, endDate: dayjs.Dayjs) => {
    setStartDate(startDate);
    setEndDate(endDate);
    // Update some data
  };
  const onSelectWorkmode = (workmode: IWorkmodeDto) => {
    console.log("workmode", workmode);
    setWorkmode(workmode);
  };
  const onConfirmChanges = async () => {
    console.log("Confirming changes");
    onClose();
  };

  return (
    <Root className="PlanningCalendarDay">
      <CloseButtonContainer>
        <IconButton aria-label="More information" disableRipple onClick={onClose}>
          <IconClose />
        </IconButton>
      </CloseButtonContainer>

      <WidgetContainer>
        <Box>
          <H2Center>When</H2Center>

          <Flex margin="-0.5rem">
            <DatePickerContainer>
              <DatePicker
                value={startDate}
                label="from"
                format="D MMM YYYY"
                variant={isMobile ? "dialog" : "inline"}
                onChange={(date: any) =>
                  onSelectDateRange(date, endDate.isBefore(date) ? date : endDate)
                }
              />
            </DatePickerContainer>
            <DatePickerContainer>
              <DatePicker
                value={endDate}
                label="to"
                minDate={startDate}
                format="D MMM YYYY"
                variant={isMobile ? "dialog" : "inline"}
                onChange={(date: any) => onSelectDateRange(startDate, date)}
              />
            </DatePickerContainer>
          </Flex>
        </Box>

        <Box>
          <H2Center>Where</H2Center>

          <H4>Where are you working from?</H4>
          {/* TODO: Handle officeCapacity */}
          <WorkmodeButtons
            disabled={false}
            workmode={workmode}
            officeCapacity={9001}
            onSelectWorkmode={onSelectWorkmode}
          />
        </Box>

        <OfficeInfoContainer>
          {/* TODO: userSite should be office dto */}
          Current office: {userOffice?.site || "N/A"}
          <br />
          x/y spots available
        </OfficeInfoContainer>

        <Box textAlign="center">
          <Button variant="contained" color="primary" onClick={onConfirmChanges}>
            Confirm changes
          </Button>
        </Box>
      </WidgetContainer>
    </Root>
  );
};
