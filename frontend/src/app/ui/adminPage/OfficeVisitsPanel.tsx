import React, { useState, useCallback, useContext } from "react";
import { useMutation } from "react-query";
import dayjs from "dayjs";

import CollapsibleTable from "./CollapsibleTable";
import { officeBookingsQueryKey } from "../../utils/reactQueryUtils";
import { useServices } from "../../services/services";
import {
  IOfficeDto,
  IUserDto,
  Workmode,
  IAdminRegistrationDto,
  IWorkmodeDto,
  IContactsDto,
  IOfficeBookingsRequestDto,
  IRegistrationIdDto,
} from "../../services/apiClientService";
import { ITableDataDto, ICollapsibleTableHead, IUserDtoMapped } from "./types";
import { VisitorsToolbar } from "./VisitorsToolbar";
import { BookingsTable } from "./BookingsTable";
import { CenteredSpinner, CenteredSpinnerContainer } from "../ux/spinner";
import { ModalContext } from "app/providers/ModalProvider";
import { AdminEditContext } from "./AdminEditContext";

interface IOfficeVisitsPanel {
  offices: IOfficeDto[];
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
  office,
  offices,
}: {
  bookings: IContactsDto;
  office: string;
  offices: IOfficeDto[];
}): ITableDataDto {
  const { people, date } = bookings;
  const mappedPeople: IUserDtoMapped[] = people.map((person: IUserDto) => ({
    name: person.name,
    email: person.email,
    checked: false,
  }));
  const { capacity } = offices.filter((o) => o.name === office)[0];

  return {
    date: dayjs(date).format("D MMM YYYY") || "",
    office,
    visitors: mappedPeople,
    utilisation: `${mappedPeople.length} people (max ${capacity})`,
  };
}

export function OfficeVisitsPanel({ offices, users }: IOfficeVisitsPanel) {
  const { apiClient, queryCache } = useServices();
  const today = dayjs().utc().startOf("day");

  const { handleModalClose } = useContext(ModalContext);

  const [startDate, setStartDate] = useState(() => today.startOf("week").add(1, "day"));
  const [endDate, setEndDate] = useState(() => today.endOf("week").subtract(1, "day"));
  const [currentOffice, setCurrentOffice] = useState("");
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
      office: currentOffice,
      startDate: startDateStr,
      endDate: endDateStr,
    });
  };

  const handleOfficeChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    setCurrentOffice(event.target.value as string);
  };

  const handleDateChange = (startDate: dayjs.Dayjs, endDate: dayjs.Dayjs) => {
    setStartDate(startDate);
    setEndDate(endDate);
  };

  const [mutateOfficeBookings, mutateOfficeBookingsRes] = useMutation(
    (req: IOfficeBookingsRequestDto) =>
      apiClient.admin.getOfficeBookings(req),
    {
      onSuccess: (data) => {
        const mappedBookings: ITableDataDto[] | undefined =
          data &&
          data.map((item: IContactsDto) =>
            mapBookingsForUI({ bookings: item, office: currentOffice, offices })
          );

        setRows(mappedBookings || []);
        queryCache.refetchQueries(officeBookingsQueryKey(currentOffice, startDateStr, endDateStr));
      },
    }
  );

  const [updateRegistrations] = useMutation(
    (request: IAdminRegistrationDto[]) => apiClient.admin.setRegistrations(request),
    {
      onSuccess: async () => {
        await mutateOfficeBookings({
          office: currentOffice,
          startDate: startDateStr,
          endDate: endDateStr,
        });
        handleModalClose();
      },
    }
  );
  const [deleteRegistrations] = useMutation(
    (request: IRegistrationIdDto[]) => apiClient.admin.deleteRegistrations(request),
    {
      onSuccess: async () => {
        await mutateOfficeBookings({
          office: currentOffice,
          startDate: startDateStr,
          endDate: endDateStr,
        });
        handleModalClose();
      },
    }
  );

  const onAddEmployee = (email: string, formatedDate: string, office: string) => {
    const date = dayjs(formatedDate).format("YYYY-MM-DD");
    const workmode: IWorkmodeDto = {
      type: Workmode.Office,
      confirmed: true,
      name: Workmode.Office,
    };
    updateRegistrations([{ date, office, workmode, email }]);
  };

  const onDeleteEmployee = (date: string) => {
    const formattedDate = dayjs(date).format("YYYY-MM-DD");
    const visitors = rows.find((row) => row.date === date)?.visitors.filter((v) => v.checked);

    const selectedUsers: IRegistrationIdDto[] = visitors
      ? visitors.map((v) => {
        return { email: v.email, date: formattedDate };
      })
      : [];
    deleteRegistrations(selectedUsers);
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
        currentSite={currentOffice}
        onDateChange={handleDateChange}
        onSiteChange={handleOfficeChange}
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
