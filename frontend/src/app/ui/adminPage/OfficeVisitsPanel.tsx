import React, { useState, useCallback, createContext, useContext } from "react";
import { useMutation } from "react-query";
import dayjs from "dayjs";

import CollapsibleTable from "./CollapsibleTable";
import { officeBookingsQueryKey, userShiftQueryKey } from "../../utils/reactQueryUtils";
import { useServices } from "../../services/services";
import {
  ICapacityDto,
  IOfficeBookingsRequestDto,
  IOfficeSpaceDto,
  IUserDto,
  Workmode,
  IAdminUserWorkmodeDto,
  IWorkmodeDto,
  IDeleteUserWorkmodeDto,
  IShiftAssignmentDto,
} from "../../services/apiClientService";
import { ITableDataDto, ICollapsibleTableHead, IUserDtoMapped } from "./types";
import { VisitorsToolbar } from "./VisitorsToolbar";
import { BookingsTable } from "./BookingsTable";
import { CenteredSpinner, CenteredSpinnerContainer } from "../ux/spinner";
import { ModalContext } from "app/providers/ModalProvider";
import { AdminEditContext } from "./AdminEditContext";

interface IOfficeVisitsPanel {
  offices: IOfficeSpaceDto[];
  users: IUserDto[];
}

const childTableHead = (isEditing: boolean): ICollapsibleTableHead[] => {
  const tableHead: ICollapsibleTableHead[] = [
    {
      title: "nr.",
    },
    {
      title: "Name",
    },
    {
      title: "Email",
    },
  ];
  if (isEditing) {
    tableHead.splice(1, 0, {
      title: "checkbox",
      checked: isEditing,
    });
  }
  return tableHead;
};

const parentTableHead = (
  isEditing: boolean,
  toggleIsEditing: () => void
): ICollapsibleTableHead[] => [
  {
    title: "",
    width: "5%",
  },
  {
    title: "Date",
    width: "10%",
  },
  {
    title: "Office",
    width: "15%",
  },
  {
    title: "Capacity utilisation",
    width: "15%",
  },
  {
    title: "",
    width: "45%",
  },
  {
    title: isEditing ? "Exit edit mode" : "Edit list",
    width: "10%",
    onClick: toggleIsEditing,
  },
];

export function mapBookingsForUI({
  bookings,
  site,
  offices,
}: {
  bookings: ICapacityDto;
  site: string;
  offices: IOfficeSpaceDto[];
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
    utilisation: `${mappedPeople.length} people (max ${maxPeople})`,
  };
}

export function OfficeVisitsPanel({ offices, users }: IOfficeVisitsPanel) {
  const { apiClient, queryCache } = useServices();
  const today = dayjs().utc().startOf("day");

  const { handleModalClose } = useContext(ModalContext);

  const [startDate, setStartDate] = useState(() => today.startOf("week").add(1, "day"));
  const [endDate, setEndDate] = useState(() => today.startOf("week").add(1, "day"));
  const userShift = queryCache.getQueryData<IShiftAssignmentDto>(userShiftQueryKey());
  const [currentSite, setCurrentSite] = useState((userShift && userShift.site) || "");
  const [rows, setRows] = useState<ITableDataDto[]>([]);
  const [isEditing, setIsEditing] = useState(false);
  const [selectAll, setSelectAll] = useState(false);

  const toggleIsEditing = useCallback(() => {
    setIsEditing(!isEditing);
  }, [isEditing, setIsEditing]);

  const startDateStr = startDate.format("YYYY-MM-DD");
  const endDateStr = endDate.format("YYYY-MM-DD");

  const handleSearch = async () => {
    await mutateOfficeBookings({
      site: currentSite,
      startDate: startDateStr,
      endDate: endDateStr,
    });
  };

  const handleSiteChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    setCurrentSite(event.target.value as string);
  };

  const handleDateChange = (startDate: dayjs.Dayjs, endDate: dayjs.Dayjs) => {
    setStartDate(startDate);
    setEndDate(endDate);
  };

  const [mutateOfficeBookings, mutateOfficeBookingsRes] = useMutation(
    ({ site, startDate, endDate }: IOfficeBookingsRequestDto) =>
      apiClient.getOfficeBookings({ site, startDate, endDate }),
    {
      onSuccess: (data) => {
        const mappedBookings: ITableDataDto[] | undefined =
          data &&
          data.map((item: ICapacityDto) =>
            mapBookingsForUI({ bookings: item, site: currentSite, offices })
          );

        setRows(mappedBookings || []);
        queryCache.refetchQueries(officeBookingsQueryKey(currentSite, startDateStr, endDateStr));
      },
    }
  );

  const [updateUserWorkmode] = useMutation(
    (request: IAdminUserWorkmodeDto[]) => apiClient.updateUserWorkmode(request),
    {
      onSuccess: async () => {
        await mutateOfficeBookings({
          site: currentSite,
          startDate: startDateStr,
          endDate: endDateStr,
        });
        handleModalClose();
      },
    }
  );
  const [deleteUserWorkmode] = useMutation(
    (request: IDeleteUserWorkmodeDto[]) => apiClient.deleteUserWorkmode(request),
    {
      onSuccess: async () => {
        await mutateOfficeBookings({
          site: currentSite,
          startDate: startDateStr,
          endDate: endDateStr,
        });
        handleModalClose();
      },
    }
  );

  const onAddEmployee = (email: string, formatedDate: string, site: string) => {
    const date = dayjs(formatedDate).format("YYYY-MM-DD");
    const workmode: IWorkmodeDto = {
      type: Workmode.Office,
      confirmed: true,
      name: Workmode.Office,
    };
    updateUserWorkmode([{ date, site, workmode, email }]);
  };

  const onDeleteEmployee = (site: string, date: string) => {
    const formattedDate = dayjs(date).format("YYYY-MM-DD");
    const dayIndex = rows.findIndex((row) => row.date === date);
    const visitors = rows.find((row) => row.date === date)?.visitors.filter((v) => v.checked);

    const selectedUsers: IDeleteUserWorkmodeDto[] = visitors
      ? visitors.map((v) => {
          return { email: v.email, date: formattedDate };
        })
      : [];
    deleteUserWorkmode(selectedUsers);
  };

  const onToggleAllRows = useCallback(
    (date: string) => {
      const toggledSelectAll = !selectAll;
      setSelectAll(toggledSelectAll);
      const dayIndex = rows.findIndex((r) => r.date === date);
      if (dayIndex !== -1) {
        const day = Object.assign({}, rows[dayIndex]);
        const newVisitors = day.visitors.map((v) => {
          return { ...v, checked: toggledSelectAll };
        });
        day.visitors = newVisitors;
        const newRows = [...rows];
        newRows[dayIndex] = day;
        setRows(newRows);
      }
    },
    [rows, setRows, selectAll, setSelectAll]
  );

  const onToggleRow = useCallback(
    (email: string, date: string) => {
      const dayIndex = rows.findIndex((r) => r.date === date);
      if (dayIndex !== -1) {
        const day = Object.assign({}, rows[dayIndex]);
        const newVisitors = day.visitors.map((v) => {
          if (v.email === email) {
            return { ...v, checked: !v.checked };
          }
          return v;
        });
        day.visitors = newVisitors;
        const newRows = [...rows];
        newRows[dayIndex] = day;
        setRows(newRows);
      }
    },
    [rows, setRows]
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
      {mutateOfficeBookingsRes.status === "loading" ? (
        <CenteredSpinnerContainer style={{ minHeight: "250px" }}>
          <CenteredSpinner />
        </CenteredSpinnerContainer>
      ) : (
        <AdminEditContext.Provider
          value={{
            isEditing,
            onToggleAllRows,
            onToggleRow,
            onAddEmployee,
            onDeleteEmployee,
            users,
          }}
        >
          <CollapsibleTable
            childComponent={BookingsTable}
            childTableHead={childTableHead(isEditing)}
            parentTableHead={parentTableHead(isEditing, toggleIsEditing)}
            empty={"No result for the selected parameters."}
            rows={rows}
          />
        </AdminEditContext.Provider>
      )}
    </>
  );
}
