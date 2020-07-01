import dayjs from "dayjs";

export function dateRange(startDate: dayjs.Dayjs, endDate: dayjs.Dayjs) {
  const arr = [];

  while (!startDate.isAfter(endDate)) {
    arr.push(startDate);
    startDate = startDate.add(1, "day");
  }

  return arr;
}

export function weeklyDateRanges(startDate: dayjs.Dayjs, endDate: dayjs.Dayjs): dayjs.Dayjs[][] {
  const arrays = [];

  while (!startDate.isAfter(endDate)) {
    // NOTE: dayjs weeks start from sunday
    const endOfWeek = startDate.endOf("week").add(1, "day").startOf("day");
    arrays.push(dateRange(startDate, endOfWeek.isBefore(endDate) ? endOfWeek : endDate));
    startDate = endOfWeek.add(1, "day");
  }

  return arrays;
}

export function monthlyDateRanges(startDate: dayjs.Dayjs, endDate: dayjs.Dayjs): dayjs.Dayjs[][] {
  const arrays = [];

  while (!startDate.isAfter(endDate)) {
    const endOfMonth = startDate.endOf("month").startOf("day");
    arrays.push(dateRange(startDate, endOfMonth.isBefore(endDate) ? endOfMonth : endDate));
    startDate = endOfMonth.add(1, "day");
  }

  return arrays;
}

export const isWeekend = (date: dayjs.Dayjs) => date.day() === 0 || date.day() === 6;
