import React, { useState } from "react";
import {
  Box,
  ExpansionPanel,
  ExpansionPanelSummary,
  ExpansionPanelDetails,
  styled,
  BoxProps,
} from "@material-ui/core";
import dayjs from "dayjs";
import { Button } from "app/ui/ux/buttons";
import { Theme } from "app/ui/ux/theme";

interface IPlanningCalendar {}

const NUM_INITIAL_WEEKS = 4;
const NUM_LOAD_MORE_WEEKS = 4;

function dateRange(startDate: dayjs.Dayjs, endDate: dayjs.Dayjs) {
  const arr = [];

  while (startDate.isBefore(endDate)) {
    arr.push(startDate);
    startDate = startDate.add(1, "day");
  }

  return arr;
}

interface IPlanningDay {
  date: dayjs.Dayjs;
  onClose: () => void;
}

const PlanningDay: React.FC<IPlanningDay> = ({ date, onClose }) => {
  return (
    <Box className="PlanningDay">
      <p>
        Aliquip ea velit officia enim id magna nisi amet veniam dolore amet. Deserunt aliquip
        nostrud velit non dolor magna occaecat duis aliqua nulla id est. Dolor et cillum nulla anim
        culpa incididunt anim.
      </p>
    </Box>
  );
};

const AccordionItem = styled(ExpansionPanel)<Theme>(({ theme }) => ({
  "&::before": { display: "none" },
  padding: "0 1rem",
  marginBottom: "0.5rem",
  background: theme.colors.white,
  boxShadow: "20px 20px 40px rgba(20, 7, 75, 0.12)",
  borderRadius: "8px",
  "&.Mui-expanded": {
    margin: 0,
    marginBottom: "0.5rem",
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
  padding: 0,
}));

const DateWrapper = styled(Box)({
  display: "flex",
  alignItems: "center",
  width: "50%",
});
const StatusWrapper = styled(Box)({
  width: "50%",
});

interface IDateNumber {
  isToday?: boolean;
}

const DateNumber = styled(Box)<Theme, IDateNumber & BoxProps>(({ theme, isToday }) => ({
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

export const PlanningCalendar: React.FC<IPlanningCalendar> = () => {
  const today = dayjs().startOf("day");
  const [expandedDate, setExpandedDate] = useState<string>();
  const [startDate, setStartDate] = useState(() => {
    const today = dayjs();
    const weekStart = today.startOf("week").add(1, "day");
    return weekStart;
  });
  const [endDate, setEndDate] = useState(() => startDate.add(NUM_INITIAL_WEEKS, "week"));

  return (
    <Box className="PlanningCalendar">
      <p>
        Velit eiusmod incididunt veniam duis. In tempor ut sit dolor. Aute sunt reprehenderit et
        occaecat laboris enim occaecat adipisicing est aute ullamco irure. Consectetur excepteur
        deserunt sit elit deserunt amet consequat nulla sit.
      </p>
      <Button
        variant="contained"
        color="primary"
        onClick={() => setStartDate(startDate.subtract(NUM_LOAD_MORE_WEEKS, "week"))}
      >
        Load more
      </Button>
      {startDate.toISOString()} - {endDate.toISOString()}
      <Box>
        {dateRange(startDate, endDate).map((date) => {
          const dateStr = date.format("YYYY-MM-DD");
          const isToday = date.isSame(today);
          return (
            <AccordionItem
              key={dateStr}
              expanded={!!expandedDate && expandedDate === dateStr}
              onChange={(_, isExpanded) => setExpandedDate(isExpanded ? dateStr : undefined)}
            >
              <AccordionTitle id={dateStr}>
                <DateWrapper>
                  <DateNumber isToday={isToday}>{date.date()}</DateNumber>
                  <DateName>{date.format("ddd")}</DateName>
                </DateWrapper>
                <StatusWrapper>Home</StatusWrapper>
              </AccordionTitle>
              <AccordionContent>
                <PlanningDay
                  date={date}
                  onClose={() => console.log(`Closing ${date.format("YYYY-MM-DD")}`)}
                />
              </AccordionContent>
            </AccordionItem>
          );
        })}
      </Box>
      <Button
        variant="contained"
        color="primary"
        onClick={() => setEndDate(endDate.add(NUM_LOAD_MORE_WEEKS, "week"))}
      >
        Load more
      </Button>
    </Box>
  );
};
