import React, { useState, useCallback, useContext } from "react";
import { useMutation, useQuery } from "react-query";

import { useTableContainerStyles, useTableHeadCellStyles, TableEmptyContainer } from "./CollapsibleTable";
import { officeBookingsQueryKey, officesQueryKey, RenderQuery, adminsQueryKey } from "../../utils/reactQueryUtils";
import { useServices } from "../../services/services";
import {
  IOfficeDto,
} from "../../services/apiClientService";
import { ITableDataDto, ICollapsibleTableHead } from "./types";
import { ModalContext } from "app/providers/ModalProvider";
import { AdminEditContext } from "./AdminEditContext";
import { TableContainer, Paper, Table, TableHead, TableRow, TableCell, TableBody, makeStyles } from "@material-ui/core";
import { HorizontalStack } from "../ux/containers";
import { Checkbox } from "../ux/checkbox";
import { TextField } from "../ux/inputs";
import { Button } from "../ux/buttons";
import AddOfficeModalContent from "./AddOfficeModalContent";
import { colors } from "../ux/theme";
import DeleteOfficeModalContent from "./DeleteOfficeModalContent";
import DeleteAdminModalContent from "./DeleteAdminsModalContent";
import AddAdminModalContent from "./AddAdminModalContent";

interface IAdminPanel {
  users: IUserDto[];
}

const tableHead = (
  isEditing: boolean,
  toggleIsEditing: () => void
): ICollapsibleTableHead[] => [
    {
      title: isEditing ? "checkbox" : "",
      width: "10%",
    },
    {
      title: "Name",
      width: "50%",
    },
    {
      title: "Email",
      width: "30%",
    },
    {
      title: isEditing ? "Exit edit mode" : "Edit list",
      width: "10%",
      onClick: toggleIsEditing,
    },
  ];

export const useRowStyles = makeStyles({
  root: {
    "& > *": {
      borderBottom: "none",
    },
    "&:nth-child(n+1)": {
      borderTop: `1px solid ${colors["deep-blue-20"]}`
    }
  }
});

export function AdminPanel({ users }: IAdminPanel) {
  const { apiClient, queryCache } = useServices();

  const { handleModalClose, handleModalOpen, setSelected } = useContext(ModalContext);

  const [rows, setRowsRaw] = useState<(IUserDto & { checked: boolean })[]>([]);
  const [isEditing, setIsEditing] = useState(false);

  const setRows = (admins: any) => setRowsRaw(admins.sort((a, b) => a.name.localeCompare(b.name)));

  const toggleIsEditing = useCallback(() => {
    setIsEditing(!isEditing);
  }, [isEditing, setIsEditing]);

  const toggleAllRows = () => {
    const isChecked = rows.reduce((b, { checked }) => b && checked, true);
    setRows(
      rows.map(r => ({ ...r, checked: !isChecked }))
    );
  };

  const toggleRow = (n: number) => {
    setRows(rows.map((r, i) => i === n ? { ...r, checked: !r.checked } : r));
  };

  const [addAdmin] = useMutation(
    (request: string) => apiClient.admin.addAdmin(request),
    {
      onSuccess: () => queryCache.refetchQueries(adminsQueryKey())
    }
  );
  const [deleteAdmins] = useMutation(
    async (requests: string[]) => {
      for(const r of requests) {
        await apiClient.admin.deleteAdmin(r);
      }
    },
    {
      onSuccess: () => queryCache.refetchQueries(adminsQueryKey())
    }
  );

  const adminsRes = useQuery(adminsQueryKey(), () =>
    apiClient.admin.getAdmins().catch(() => null).then(admins => {
      setRows(admins as any)
      return admins;
    })
  );

  const tableContainerClasses = useTableContainerStyles();
  const tableHeadClasses = useTableHeadCellStyles();
  const classes = useRowStyles();

  const onDeleteAdmins = () => {
    const emails = rows.filter(x => x.checked).map(x => x.email);
    deleteAdmins(emails);
    handleModalClose();
  };

  return (
    <RenderQuery
      query={adminsRes}
      onLoading={(data, children) => children(data || ({} as any), true)}
      onError={(error, children) => children({} as any, false, error)}
    >{ () =>

    <TableContainer
      className={tableContainerClasses.root}
      component={Paper}
    >
      <Table aria-label="collapsible table">
        <TableHead>
          <TableRow>
            {tableHead(isEditing, toggleIsEditing).map(({ align = "left", title, width = "", onClick, }: ICollapsibleTableHead, i) =>
              <TableCell
                key={title + i}
                style={{ width }}
                className={onClick ? tableHeadClasses.button : tableHeadClasses.root}
                align={align}
                onClick={onClick}
              >{title === "checkbox" ? <Checkbox onClick={() => toggleAllRows()} /> : title }</TableCell>
            )}
          </TableRow>
        </TableHead>
        <TableBody>
          {
            rows.length !== 0 && (
              rows.map((row, i) => (
                <TableRow
                  className={classes.root}
                  key={i + "1"}
                >
                  <TableCell key="checkbox">
                    {isEditing && <Checkbox checked={Boolean(row.checked)} onClick={() => toggleRow(i)} /> }
                  </TableCell>
                  <TableCell key="name">{row.name}</TableCell>
                  <TableCell key="email">{row.email}</TableCell>
                </TableRow>
              ))
            )
          }
          {isEditing ? (
          <TableRow>
            <TableCell colSpan={3}>
              <HorizontalStack spacing="0.8rem" marginTop="1.25rem">
                <Button
                  variant="contained"
                  color="primary"
                  onClick={() => {
                    handleModalOpen();
                    setSelected(
                      <DeleteAdminModalContent
                        onDelete={onDeleteAdmins}
                      />
                    );
                  }}
                >
                  Remove admins
                </Button>
                <Button
                  variant="contained"
                  color="secondary"
                  onClick={() => {
                    handleModalOpen();
                    setSelected(
                      <AddAdminModalContent
                        users={users}
                        onAdd={email => {
                          addAdmin(email);
                          handleModalClose();
                        }}
                      />
                    );
                  }}
                >
                  Add an administrator
                </Button>
              </HorizontalStack>
            </TableCell>
          </TableRow>
        ) : null}

        </TableBody>
      </Table>
      {
        rows.length === 0 && <TableEmptyContainer empty={"No result for the selected parameters."} />
      }
    </TableContainer>
    }</RenderQuery>
  );
}
