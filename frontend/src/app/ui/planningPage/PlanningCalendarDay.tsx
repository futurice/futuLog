import React, { useState } from "react";
import dayjs from "dayjs";
import { styled, useMediaQuery, Box } from "@material-ui/core";
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
  IUserWorkmodeDto,
  IUserDto,
  IBookedDto
} from "app/services/apiClientService";
import { useServices } from "app/services/services";
import {
  userShiftQueryKey,
  officesQueryKey,
  officeBookingsQueryKey,
  RenderQuery,
  userQueryKey,
} from "app/utils/reactQueryUtils";
import { useQuery } from "react-query";
import { Button, IconButton } from "app/ui/ux/buttons";
import { dateRange, isWeekend } from "app/utils/dateUtils";

interface IPlanningCalendarDay {
  date: dayjs.Dayjs;
  workmode: Workmode;
  isLoading?: boolean;
  isExpanded?: boolean;
  onSelectWorkmodes: (workmodes: IUserWorkmodeDto[]) => void;
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

const ConfirmButtonContainer = styled("div")<Theme>(({ theme }) => ({
  textAlign: "center",
  [theme.breakpoints.down("sm")]: {
    position: "fixed",
    zIndex: 1,
    bottom: 0,
    left: 0,
    right: 0,
    padding: "1rem",
    borderTop: `1px solid ${theme.colors["deep-blue-20"]}`,
    backgroundColor: theme.colors.white,
  },
}));

function getOfficeCapacity(office: IOfficeSpaceDto, bookings: IBookedDto[]) {
  if (!bookings.length) {
    return office.maxPeople;
  }
  const capacities = bookings.map((booking) => office.maxPeople - booking.users.length);
  return Math.min(...capacities);
}

export const PlanningCalendarDay: React.FC<IPlanningCalendarDay> = ({
  date,
  workmode,
  isLoading,
  isExpanded,
  onSelectWorkmodes,
  onClose,
}) => {
  const { apiClient, queryCache } = useServices();
  const [startDate, setStartDate] = useState(date);
  const [endDate, setEndDate] = useState(date);
  const [localWorkmode, setLocalWorkmode] = useState<IWorkmodeDto>({ type: workmode });
  const [isChanged, setIsChanged] = useState(false);
  const isMobile = useMediaQuery((theme: Theme) => theme.breakpoints.down("sm"));
  const startDateStr = startDate.format("YYYY-MM-DD");
  const endDateStr = endDate.format("YYYY-MM-DD");

  // These should be pre-loaded by AppRoutes
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const user = queryCache.getQueryData<IUserDto>(userQueryKey())!;
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const userShift = queryCache.getQueryData<IShiftAssignmentDto>(userShiftQueryKey())!;
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const offices = queryCache.getQueryData<IOfficeSpaceDto[]>(officesQueryKey())!;
  const userOffice = (offices || []).find((office) => userShift && office.site === userShift.site);

  const officeBookingsRes = useQuery(
    userOffice && officeBookingsQueryKey(userOffice.site, startDateStr, endDateStr),
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    () => apiClient.getOfficeBookings(userOffice!.site, startDateStr, endDateStr)
  );

  const onSelectDateRange = (startDate: dayjs.Dayjs, endDate: dayjs.Dayjs) => {
    setStartDate(startDate);
    setEndDate(endDate);
    setIsChanged(true);
  };
  const onSelectLocalWorkmode = (workmode: IWorkmodeDto) => {
    setLocalWorkmode(workmode);
    setIsChanged(true);
  };
  const onConfirmChanges = async () => {
    const workmodes = dateRange(startDate, endDate)
      .filter((date) => !isWeekend(date))
      .map(
        (date) =>
          ({
            userEmail: user.email,
            site: userOffice?.site,
            date: date.format("YYYY-MM-DD"),
            workmode: localWorkmode,
          } as IUserWorkmodeDto)
      );
    await onSelectWorkmodes(workmodes);
    setIsChanged(false);
  };

  return (
    <Root className="PlanningCalendarDay">
      <CloseButtonContainer>
        <IconButton aria-label="More information" onClick={onClose}>
          <IconClose />
        </IconButton>
      </CloseButtonContainer>

      <WidgetContainer>
        <RenderQuery
          query={officeBookingsRes}
          onError={(error) => <H2Center>{error.message}</H2Center>}
          onLoading={(data, children) => children(data || [])}
        >
          {(officeBookings) => {
            const officeCapacity = userOffice ? getOfficeCapacity(userOffice, officeBookings) : 0;

            return (
              <>
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
                    disabled={officeBookingsRes.status === "loading"}
                    workmode={localWorkmode}
                    officeCapacity={officeCapacity}
                    onSelectWorkmode={onSelectLocalWorkmode}
                  />
                </Box>

                <OfficeInfoContainer>
                  Current office: {userOffice?.site || "N/A"}
                  <br />
                  {userOffice ? (
                    startDate.isSame(endDate) ? (
                      <>
                        {officeCapacity}/{userOffice.maxPeople} spots available
                      </>
                    ) : officeCapacity <= 0 ? (
                      "No spots available"
                    ) : (
                      "Multiple slots available"
                    )
                  ) : (
                    ""
                  )}
                </OfficeInfoContainer>

                {/*
                  NOTE: We use explicit `isExpanded` flag here due to collapsible
                  panels performing a transition animation. We want to hide the button
                  immediately in mobile view
                */}
                {isExpanded && isChanged && (
                  <ConfirmButtonContainer>
                    <Button
                      disabled={isLoading}
                      variant="contained"
                      color="primary"
                      onClick={onConfirmChanges}
                    >
                      Confirm changes
                    </Button>
                  </ConfirmButtonContainer>
                )}
              </>
            );
          }}
        </RenderQuery>
      </WidgetContainer>
    </Root>
  );
};
