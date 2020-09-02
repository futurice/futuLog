import React, { useState, useCallback, createContext } from "react";
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
import { StyledModal } from "../ux/modal"
import AddEmployeeModalContent from './AddEmployeeModalContent';


interface IOfficeVisitsPanel {
  offices: IOfficeSpaceDto[];
  users: IUserDto[]
}

interface IEditOfficeVisitsContext {
  isEditing: boolean,
  onToggleAllRows: (date: string) => void,
  onToggleRow: (email: string, date: string) => void,
}

export const EditOfficeVisitsContext = createContext<IEditOfficeVisitsContext>({
  isEditing: false,
  onToggleAllRows: (date: string) => { },
  onToggleRow: (email: string, date: string) => { },
});

const childTableHead = (isEditing: boolean): ICollapsibleTableHead[] => {
  const tableHead: ICollapsibleTableHead[] = [
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
  if (isEditing) {
    tableHead.splice(1, 0, {
      title: 'checkbox',
      checked: isEditing,
    })
  }
  return tableHead;
};

const parentTableHead = (isEditing: boolean, toggleIsEditing: () => void): ICollapsibleTableHead[] => ([
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
    width: "45%"
  },
  {
    title: isEditing ? "Exit edit mode" : "Edit list",
    width: "10%",
    onClick: toggleIsEditing,
  }
]);

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
    email: person.email,
    checked: false,
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
  offices,
  users
}: IOfficeVisitsPanel) {
  const { apiClient } = useServices();
  const today = dayjs().utc().startOf("day");

  const [startDate, setStartDate] = useState(() => today.startOf("week").add(1, "day"));
  const [endDate, setEndDate] = useState(() => today.startOf("week").add(1, "day"));
  const [currentSite, setCurrentSite] = useState((offices && offices[2].site) || "");
  const [rows, setRows] = useState<ITableDataDto[]>([]);
  const [isEditing, setIsEditing] = useState(false);
  const [selectAll, setSelectAll] = useState(false);
  const toggleIsEditing = useCallback(() => {
    setIsEditing(!isEditing);
  },
    [isEditing, setIsEditing],
  );


  const [currentUser, setCurrentUser] = useState({ name: "", email: "" });

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

  const handleUserChange = (event: React.ChangeEvent<{}>, value: any | null) => {
    if (value) {
      setCurrentUser({ ...currentUser, name: value.label, email: value.value })
      console.log("EMAIL:", value.value, "NAME:", value.label);
    }
    return null
  }

  const [mutateOfficeBookings, mutateOfficeBookingsRes] = useMutation(
    ({ site, startDate, endDate }: IOfficeBookingsRequestDto) => apiClient.getOfficeBookings({ site, startDate, endDate }),
    {
      onSuccess: (data) => {
        const mappedBookings: ITableDataDto[] | undefined = data && data.map(
          (item: ICapacityDto) => mapBookingsForUI({ bookings: item, site: currentSite, offices }));

        setRows(mappedBookings || []);
        queryCache.refetchQueries(officeBookingsQueryKey(currentSite, startDateStr, endDateStr))
      },
    }
  );

  const onToggleAllRows = useCallback((date: string) => {
    const toggledSelectAll = !selectAll;
    setSelectAll(toggledSelectAll);
    const dayIndex = rows.findIndex(r => r.date === date);
    if (dayIndex !== -1) {
      const day = Object.assign({}, rows[dayIndex]);
      const newVisitors = day.visitors.map(v => {
        return { ...v, checked: toggledSelectAll }
      });
      day.visitors = newVisitors;
      const newRows = [...rows];
      newRows[dayIndex] = day;
      setRows(newRows);
    }
  },
    [rows, setRows, selectAll, setSelectAll],
  );

  const onToggleRow = useCallback(
    (email: string, date: string) => {
      const dayIndex = rows.findIndex(r => r.date === date);
      if (dayIndex !== -1) {
        const day = Object.assign({}, rows[dayIndex]);
        const newVisitors = day.visitors.map(v => {
          if (v.email === email) {
            return { ...v, checked: !v.checked }
          }
          return v;
        });
        day.visitors = newVisitors;
        const newRows = [...rows];
        newRows[dayIndex] = day;
        setRows(newRows);
      }
    },
    [rows, setRows],
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
            <EditOfficeVisitsContext.Provider
              value={{
                isEditing,
                onToggleAllRows,
                onToggleRow,
              }}
            >
              <CollapsibleTable
                childComponent={BookingsTable}
                childTableHead={childTableHead(isEditing)}
                parentTableHead={parentTableHead(isEditing, toggleIsEditing)}
                empty={"No result for the selected parameters."}
                rows={rows}
                editUserButtons={true}
              />
            </EditOfficeVisitsContext.Provider>
          )
      }
      <StyledModal>
        <AddEmployeeModalContent users={users} onUserChange={handleUserChange} />
      </StyledModal>
    </>
  );
}
