import React, { useState } from "react";
import { queryCache, useMutation } from "react-query";
import dayjs from "dayjs";

import CollapsibleTable from "./CollapsibleTable";
import { officeBookingsQueryKey } from "../../utils/reactQueryUtils";
import { useServices } from "../../services/services";
import {
  ICapacityDto,
  IOfficeBookingsRequestDto,
  IOfficeSpaceDto,
  IUserDto
} from "../../services/apiClientService";
import { ITableDataDto, ICollapsibleTableHead, IUserDtoMapped } from "./types";
import { VisitorsToolbar } from "./VisitorsToolbar";
import { BookingsTable } from "./BookingsTable";
import { CenteredSpinner, CenteredSpinnerContainer } from "../ux/spinner";

interface IOfficeVisitsPanel {
  offices: IOfficeSpaceDto[];
}

const childTableHead: ICollapsibleTableHead[] = [
  {
    title: "nr."
  },
  {
    title: "Name"
  },
  {
    title: "Email"
  }
];

const parentTableHead: ICollapsibleTableHead[] = [
  {
    title: "",
    width: "5%"
  },
  {
    title: "Date",
    width: "10%"
  },
  {
    title: "Office",
    width: "15%"
  },
  {
    title: "Capacity utilisation",
    width: "15%"
  },
  {
    title: "",
    width: "55%"
  }
];

export function mapBookingsForUI({
  bookings,
  site,
  offices
}: {
  bookings: ICapacityDto,
  site: string,
  offices: IOfficeSpaceDto[]
}): ITableDataDto {
  const { people, date } = bookings;
  const mappedPeople: IUserDtoMapped[] = people.map((person: IUserDto) => ({
    name: `${person.first_name} ${person.last_name}`,
    email: person.email
  }));
  const { maxPeople } = offices.filter((office) => office.site === site)[0];

  return {
    date: dayjs(date).format("D MMM YYYY") || "",
    site,
    visitors: mappedPeople,
    utilisation: `${mappedPeople.length} people (max ${maxPeople})`
  };
}

export function OfficeVisitsPanel({
  offices
}: IOfficeVisitsPanel) {
  const { apiClient } = useServices();
  const today = dayjs().utc().startOf("day");

  const [startDate, setStartDate] = useState(() => today.startOf("week").add(1, "day"));
  const [endDate, setEndDate] = useState(() => today.startOf("week").add(1, "day"));
  const [currentSite, setCurrentSite] = useState((offices && offices[2].site) || "");
  const [rows, setRows] = useState<ITableDataDto[]>([]);

  const startDateStr = startDate.format("YYYY-MM-DD");
  const endDateStr = endDate.format("YYYY-MM-DD");

  const handleSearch = async () => {
    await mutateOfficeBookings({
      site: currentSite,
      startDate: startDateStr,
      endDate: endDateStr
    });
  }

  const handleSiteChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    setCurrentSite(event.target.value as string);
  }

  const handleDateChange = (startDate: dayjs.Dayjs, endDate: dayjs.Dayjs) => {
    setStartDate(startDate);
    setEndDate(endDate);
  }

  const [mutateOfficeBookings, mutateOfficeBookingsRes] = useMutation(
    ({ site, startDate, endDate }: IOfficeBookingsRequestDto) => apiClient.getOfficeBookings({ site, startDate, endDate}),
    {
      onSuccess: (data) => {
        const mappedBookings: ITableDataDto[] | undefined = data && data.map(
          (item: ICapacityDto) => mapBookingsForUI({ bookings: item, site: currentSite, offices }));

        setRows(mappedBookings || []);
        queryCache.refetchQueries(officeBookingsQueryKey(currentSite, startDateStr, endDateStr))
      },
    }
  );

  return (
    <>
      <VisitorsToolbar
        tableData={rows}
        startDate={startDate}
        endDate={endDate}
        offices={offices}
        currentSite={currentSite}
        onDateChange={handleDateChange}
        onSiteChange={handleSiteChange}
        onSearch={handleSearch}
      />
      {
        mutateOfficeBookingsRes.status === "loading" ? (
          <CenteredSpinnerContainer style={{ minHeight: "250px" }}>
            <CenteredSpinner />
          </CenteredSpinnerContainer>
        ) : (
          <CollapsibleTable
            childComponent={BookingsTable}
            childTableHead={childTableHead}
            parentTableHead={parentTableHead}
            empty={"No result for the selected parameters."}
            rows={rows}
          />
        )
      }
    </>
  );
}
