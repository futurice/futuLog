import React, { useState, useEffect, useRef } from "react";
import {
  Box,
  ExpansionPanel,
  ExpansionPanelSummary,
  ExpansionPanelDetails,
  styled,
  BoxProps,
  ExpansionPanelProps,
} from "@material-ui/core";
import dayjs from "dayjs";
import utc from "dayjs/plugin/utc";
import { useServices } from "app/services/services";
import { Button } from "app/ui/ux/buttons";
import { Theme } from "app/ui/ux/theme";
import { H3 } from "app/ui/ux/text";
import { PlanningCalendarDay } from "app/ui/planningPage/PlanningCalendarDay";

dayjs.extend(utc);

interface IPlanningCalendar {
  onChangeVisibleMonth: (month: string) => void;
}

const NUM_INITIAL_WEEKS = 4;
const NUM_LOAD_MORE_WEEKS = 4;

function dateRange(startDate: dayjs.Dayjs, endDate: dayjs.Dayjs) {
  const arr = [];

  while (!startDate.isAfter(endDate)) {
    arr.push(startDate);
    startDate = startDate.add(1, "day");
  }

  return arr;
}

function weeklyDateRanges(startDate: dayjs.Dayjs, endDate: dayjs.Dayjs): dayjs.Dayjs[][] {
  const arrays = [];

  while (!startDate.isAfter(endDate)) {
    // NOTE: dayjs weeks start from sunday
    const endOfWeek = startDate.endOf("week").add(1, "day").startOf("day");
    arrays.push(dateRange(startDate, endOfWeek.isBefore(endDate) ? endOfWeek : endDate));
    startDate = endOfWeek.add(1, "day");
  }

  return arrays;
}

function monthlyDateRanges(startDate: dayjs.Dayjs, endDate: dayjs.Dayjs): dayjs.Dayjs[][] {
  const arrays = [];

  while (!startDate.isAfter(endDate)) {
    const endOfMonth = startDate.endOf("month").startOf("day");
    arrays.push(dateRange(startDate, endOfMonth.isBefore(endDate) ? endOfMonth : endDate));
    startDate = endOfMonth.add(1, "day");
  }

  return arrays;
}

const isWeekend = (date: dayjs.Dayjs) => date.day() === 0 || date.day() === 6;

// NOTE: This padding needs to be on the component level instead of
// the page-level where it technically should belong. This is because
// there's some evil hacking going on to get the automatic scrolling
// to current week to work with the sticky header. This padding needs
// to be taken into account when we calculate the scrolling offset
// and for some reason (at least on FF), the calculations go wrong when
// this padding exists on the page level.
const Root = styled("div")({
  padding: "2.5rem 0",
});

interface IAccordionItem extends ExpansionPanelProps {
  isPast?: boolean;
}

const AccordionItem = styled(({ isPast, ...props }) => <ExpansionPanel {...props} />)<
  Theme,
  IAccordionItem
>(({ theme, isPast }) => ({
  "&::before": { display: "none" },
  padding: "0 1rem",
  marginBottom: "0.5rem",
  background: theme.colors.white,
  opacity: isPast ? 0.5 : 1,
  boxShadow: "20px 20px 40px rgba(20, 7, 75, 0.12)",
  borderRadius: "8px",
  "&.Mui-expanded": {
    margin: 0,
    marginBottom: "0.5rem",
  },
  "&.Mui-disabled": {
    backgroundColor: theme.colors["deep-blue-10"],
  },
}));

const AccordionTitle = styled(ExpansionPanelSummary)<Theme>({
  padding: "0.75rem 0",
  fontFamily: "Futurice",
  "&.Mui-expanded": {
    minHeight: 0,
  },
  "& > .MuiExpansionPanelSummary-content": {
    alignItems: "center",
  },
  "& > .MuiExpansionPanelSummary-content, & > .MuiExpansionPanelSummary-content.Mui-expanded": {
    margin: 0,
  },
});

const AccordionContent = styled(ExpansionPanelDetails)<Theme>(({ theme }) => ({
  borderTop: `1px solid ${theme.colors["deep-blue-20"]}`,
  padding: "1rem 0.5rem 2rem",
}));

const DateWrapper = styled(Box)({
  display: "flex",
  alignItems: "center",
  width: "50%",
});

const StatusWrapper = styled(Box)({
  width: "50%",
});

const MonthTitle = styled(H3)({
  marginBottom: "0.5rem",
});

interface IDateNumber {
  isToday?: boolean;
}

const DateNumber = styled(({ isToday, ...props }) => <Box {...props} />)<
  Theme,
  IDateNumber & BoxProps
>(({ theme, isToday }) => ({
  display: "flex",
  alignItems: "center",
  justifyContent: "flex-end",
  width: "2rem",
  height: "2rem",
  fontSize: "1.5rem",
  lineHeight: 1.25,
  fontWeight: "bold",
  borderRadius: isToday ? "50%" : "0",
  border: isToday ? `1px solid ${theme.colors["deep-blue-90"]}` : "none",
}));

const DateName = styled(Box)<Theme>({
  padding: "0 0.5rem",
  fontSize: "1rem",
  lineHeight: 1.25,
  fontWeight: "bold",
});

export const PlanningCalendar: React.FC<IPlanningCalendar> = ({ onChangeVisibleMonth }) => {
  const { history } = useServices();
  const rootEl = useRef<HTMLDivElement>(null);
  const today = dayjs().utc().startOf("day");
  const [expandedDate, setExpandedDate] = useState<string>();
  // NOTE: dayjs weeks start from sunday, hence we'll add 1 day
  const [startDate, setStartDate] = useState(() => today.startOf("week").add(1, "day"));
  const [endDate, setEndDate] = useState(() =>
    startDate.add(NUM_INITIAL_WEEKS, "week").subtract(1, "day")
  );
  const startDateStr = startDate.format("YYYY-MM-DD");
  const endDateStr = endDate.format("YYYY-MM-DD");

  const getStickyHeaderOffset = (rootEl: HTMLElement) => {
    // Calculate the absolute position of the root element which we can
    // use as offset for the element that we want to scroll into view
    const rootRect = rootEl.getBoundingClientRect();
    const offset = rootRect ? rootRect.top + window.pageYOffset : 0;
    return offset;
  };

  //
  // Handle initial scrolling taking into account the sticky headers in the pages,
  // even in mobile mode where we don't show the calendar header
  useEffect(() => {
    const scrollDate = history.location.hash.slice(1) || startDateStr;
    const scrollEl = rootEl.current?.querySelector(`#date-${scrollDate}`);
    if (scrollEl && rootEl.current) {
      const offset = getStickyHeaderOffset(rootEl.current);
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      scrollEl.style.scrollMarginTop = `${offset}px`;
      scrollEl.scrollIntoView();
    }
  }, []); // eslint-disable-line

  //
  // Handle scroll changes that affect the month name displayed in calendar header
  useEffect(() => {
    // Recall the initial month
    onChangeVisibleMonth(startDate.format("MMMM YYYY"));

    const monthElements = rootEl.current?.querySelectorAll("[data-month]");
    if (!monthElements) {
      return;
    }
    const monthVisibilities: Record<string, boolean> = Object.fromEntries(
      Array.from(monthElements.values()).map((el) => [el.getAttribute("data-month"), false])
    );

    const offset = rootEl.current ? getStickyHeaderOffset(rootEl.current) : 0;

    const observer = new IntersectionObserver(
      (entries) => {
        for (const entry of entries) {
          // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
          const month = entry.target.getAttribute("data-month")!;
          monthVisibilities[month] = entry.isIntersecting;
        }

        for (const [month, isVisible] of Object.entries(monthVisibilities)) {
          if (isVisible) {
            onChangeVisibleMonth(month);
            break;
          }
        }
      },
      { rootMargin: `-${offset}px` }
    );
    monthElements.forEach((el) => observer.observe(el));

    return () => observer.disconnect();
  }, [startDateStr, endDateStr, onChangeVisibleMonth]); // eslint-disable-line

  //
  // View

  return (
    <Root className="PlanningCalendar" ref={rootEl}>
      <Box display="flex" paddingBottom="0.5rem" justifyContent="center">
        <Button
          variant="contained"
          color="primary"
          onClick={() =>
            setStartDate(startDate.subtract(NUM_LOAD_MORE_WEEKS, "week").startOf("day"))
          }
        >
          Load more
        </Button>
      </Box>

      <p>
        {startDate.toISOString()} - {endDate.toISOString()}
      </p>

      {monthlyDateRanges(startDate, endDate).map((monthDates) => {
        const monthName = monthDates[0].format("MMMM YYYY");

        return (
          <Box key={monthDates[0].unix()} data-month={monthName}>
            <MonthTitle>{monthName}</MonthTitle>
            {weeklyDateRanges(monthDates[0], monthDates[monthDates.length - 1]).map((dates) => (
              <Box key={dates[0].unix()} paddingBottom="1.5rem">
                {dates.map((date) => {
                  const dateStr = date.format("YYYY-MM-DD");

                  return (
                    <AccordionItem
                      key={dateStr}
                      disabled={isWeekend(date)}
                      isPast={date.isBefore(today)}
                      expanded={!!expandedDate && expandedDate === dateStr}
                      TransitionProps={{ mountOnEnter: true, unmountOnExit: true }}
                      onChange={(_, isExpanded) =>
                        setExpandedDate(isExpanded ? dateStr : undefined)
                      }
                    >
                      <AccordionTitle className="AccordionTitle" id={`date-${dateStr}`}>
                        <DateWrapper>
                          <DateNumber isToday={date.isSame(today)}>{date.date()}</DateNumber>
                          <DateName>{date.format("ddd")}</DateName>
                        </DateWrapper>
                        <StatusWrapper>Home</StatusWrapper>
                      </AccordionTitle>
                      <AccordionContent>
                        <PlanningCalendarDay
                          date={date}
                          onClose={() => setExpandedDate(undefined)}
                        />
                      </AccordionContent>
                    </AccordionItem>
                  );
                })}
              </Box>
            ))}
          </Box>
        );
      })}

      <Box display="flex" justifyContent="center">
        <Button
          variant="contained"
          color="primary"
          onClick={() => setEndDate(endDate.add(NUM_LOAD_MORE_WEEKS, "week").startOf("day"))}
        >
          Load more
        </Button>
      </Box>
    </Root>
  );
};
