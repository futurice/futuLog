import React, { useState } from "react";
import { Box } from "@material-ui/core";
import dayjs from "dayjs";
import { Button } from "app/ui/ux/buttons";

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
  date: string;
  onClose: () => void;
}

const PlanningDay: React.FC<IPlanningDay> = ({ date, onClose }) => {
  return (
    <Box className="PlanningDay">
      Aliquip ea velit officia enim id magna nisi amet veniam dolore amet. Deserunt aliquip nostrud
      velit non dolor magna occaecat duis aliqua nulla id est. Dolor et cillum nulla anim culpa
      incididunt anim.
    </Box>
  );
};

export const PlanningCalendar: React.FC<IPlanningCalendar> = () => {
  const today = dayjs().startOf("day");
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

      <ul>
        <li>Start date: {startDate.toISOString()}</li>
        <li>End date: {endDate.toISOString()}</li>
        {dateRange(startDate, endDate).map((date) => (
          <li key={date.toISOString()} id={date.format("YYYY-MM-DD")}>
            <p>
              {date.date()} {date.format("ddd")} ({date.toISOString()}) {date.isSame(today) && "*"}
            </p>
          </li>
        ))}
      </ul>

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
