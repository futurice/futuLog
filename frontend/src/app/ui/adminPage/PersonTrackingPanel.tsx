import React, { useState, useCallback, useContext } from "react";
import { queryCache, useMutation } from "react-query";
import dayjs from "dayjs";

import CollapsibleTable from "./CollapsibleTable";
import { userContactsQueryKey } from "../../utils/reactQueryUtils";
import { useServices } from "../../services/services";
import {
  ICapacityDto,
  IOfficeSpaceDto,
  IUserDataRequestDto,
  IUserDto,
  IWorkmodeDto,
  Workmode,
  IDeleteUserWorkmodeDto,
  IAdminUserWorkmodeDto,
} from "../../services/apiClientService";
import { ICollapsibleTableHead, ITableDataDto, IUserDtoMapped } from "./types";
import { DAYS_RANGE_OPTIONS, TrackingToolbar } from "./TrackingToolbar";
import { BookingsTable } from "./BookingsTable";
import { CenteredSpinner, CenteredSpinnerContainer } from "../ux/spinner";
import { AdminEditContext } from "./AdminEditContext";
import { ModalContext } from "app/providers/ModalProvider";

interface IPersonTrackingPanel {
  users: IUserDto[];
  offices?: IOfficeSpaceDto[];
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
      width: "10%",
    },
    {
      title: "Person",
      width: "15%",
    },
    {
      title: "Capacity utilisation",
      width: "10%",
    },
    {
      title: "",
      width: "40%",
    },
    {
      title: isEditing ? "Exit edit mode" : "Edit list",
      width: "10%",
      onClick: toggleIsEditing,
    },
  ];

const mapBookingsForUI = ({
  bookings,
  user,
  offices,
}: {
  bookings: ICapacityDto;
  user: string;
  offices: IOfficeSpaceDto[];
}): ITableDataDto => {
  const { people, date, site } = bookings;
  const mappedPeople: IUserDtoMapped[] = people.map((person: IUserDto) => ({
    name: `${person.first_name} ${person.last_name}`,
    email: person.email,
    checked: false,
  }));
  const { maxPeople } = offices.filter((office) => office.site === site)[0];

  return {
    date: dayjs(date).format("D MMM YYYY") || "",
    site: site || "",
    person: user,
    visitors: mappedPeople,
    utilisation: `${mappedPeople.length} people (max ${maxPeople})`,
  };
};

export function PersonTrackingPanel({ users, offices }: IPersonTrackingPanel) {
  const { apiClient } = useServices();
  const today = dayjs().utc().startOf("day");

  const [startDate, setStartDate] = useState(() => today.startOf("week").add(1, "day"));
  const [endDate, setEndDate] = useState(() => today.startOf("week").add(1, "day"));
  const [currentUser, setCurrentUser] = useState("");
  const [range, setRange] = useState(DAYS_RANGE_OPTIONS[1].value);
  const [rows, setRows] = useState<ITableDataDto[]>([]);
  const [isEditing, setIsEditing] = useState(false);
  const [selectAll, setSelectAll] = useState(false);

  const { handleModalClose } = useContext(ModalContext);

  const toggleIsEditing = useCallback(() => {
    setIsEditing(!isEditing);
  }, [isEditing, setIsEditing]);

  const handleSearch = async () => {
    await mutateUserContacts({
      user: currentUser,
      startDate: startDate.subtract(range, "day").format("YYYY-MM-DD"),
      endDate: endDate.format("YYYY-MM-DD"),
    });
  };

  const handleUserChange = (event: React.ChangeEvent<{}>, value: any | null) => {
    setCurrentUser(value.value);
  };

  const handleRangeChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    setRange(event.target.value as number);
  };

  const handleDateChange = (startDate: dayjs.Dayjs, endDate: dayjs.Dayjs) => {
    setStartDate(startDate);
    setEndDate(endDate);
  };

  const [mutateUserContacts, mutateUserContactsRes] = useMutation(
    ({ user, startDate, endDate }: IUserDataRequestDto) =>
      apiClient.getUserContacts({ user, startDate, endDate }),
    {
      onSuccess: (data) => {
        const user = users.find(({ email }) => currentUser === email);
        if (user) {
          const mappedBookings: ITableDataDto[] | undefined =
            data &&
            data.map((item: ICapacityDto) => {
              return mapBookingsForUI({
                bookings: item,
                user: `${user?.first_name} ${user?.last_name}`,
                offices: offices || [],
              });
            });

          setRows(mappedBookings || []);

          queryCache.refetchQueries(
            userContactsQueryKey(
              currentUser,
              startDate.subtract(range, "day").format("YYYY-MM-DD"),
              endDate.format("YYYY-MM-DD")
            )
          );
        }
      },
    }
  );

  const startDateStr = startDate.format("YYYY-MM-DD");
  const endDateStr = endDate.format("YYYY-MM-DD");

  const [updateUserWorkmode] = useMutation(
    (request: IAdminUserWorkmodeDto[]) => apiClient.updateUserWorkmode(request),
    {
      onSuccess: async () => {
        await mutateUserContacts({
          user: currentUser,
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
        await mutateUserContacts({
          user: currentUser,
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

  const onDeleteEmployee = (date: string) => {
    const formattedDate = dayjs(date).format("YYYY-MM-DD");
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
      <TrackingToolbar
        tableData={rows}
        startDate={startDate}
        range={range}
        users={users}
        currentUser={currentUser}
        onDateChange={handleDateChange}
        onUserChange={handleUserChange}
        onRangeChange={handleRangeChange}
        onSearch={handleSearch}
      />
      {mutateUserContactsRes.status === "loading" ? (
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
