import React, { useContext } from "react";
import { Table, TableBody, TableHead, TableRow } from "@material-ui/core";
import { makeStyles } from "@material-ui/core/styles";
import { Checkbox } from "../ux/checkbox";
import { TableCell } from "./styled";
import { ICollapsibleTableHead } from "./types";
import { mapBookingsForUI } from "./OfficeVisitsPanel";
import { AdminEditContext } from "./AdminEditContext";
import { Button } from "../ux/buttons";
import { HorizontalStack } from "../ux/containers";
import { ModalContext } from "../../providers/ModalProvider";
import AddEmployeeModalContent from "./AddEmployeeModalContent";
import DeleteEmployeeModalContent from "./DeleteEmployeeModalContent";
import { colors } from "../ux/theme";

interface IBookingsTable {
  row: ReturnType<typeof mapBookingsForUI>;
  head: ICollapsibleTableHead[];
}

const useChildTableStyles = makeStyles({
  root: {
    boxShadow: "none",
    backgroundColor: "transparent",
  },
});

const useChildCellStyles = makeStyles({
  root: {
    border: "none",
  },
});

const useTableHeadCellStyles = makeStyles({
  root: {
    borderTop: `1px solid ${colors["deep-blue-20"]}`,
    borderBottom: `1px solid ${colors["deep-blue-20"]}`,
  },
});

export function BookingsTable({ row, head }: IBookingsTable) {
  const tableClasses = useChildTableStyles();
  const cellClasses = useChildCellStyles();
  const headCellClasses = useTableHeadCellStyles();
  const {
    isEditing,
    onToggleAllRows,
    onToggleRow,
    onAddEmployee,
    onDeleteEmployee,
    users,
  } = useContext(AdminEditContext);
  const { handleModalOpen, setModalState, setSelected } = useContext(ModalContext);

  return (
    <Table className={tableClasses.root} size="small" aria-label="visitors">
      <TableHead>
        <TableRow>
          {head.map(({ align = "left", title, checked }: ICollapsibleTableHead) => (
            <TableCell key={title} className={headCellClasses.root} align={align}>
              {checked ? (
                <Checkbox value={checked} onClick={() => onToggleAllRows(row.date)} />
              ) : (
                  title
                )}
            </TableCell>
          ))}
        </TableRow>
      </TableHead>
      <TableBody>
        {row.visitors.map(({ name, email, checked }, i) => (
          <TableRow key={email}>
            <TableCell
              className={cellClasses.root}
              component="td"
              scope="row"
              style={{ paddingLeft: "20px" }}
            >
              {i + 1}
            </TableCell>
            {isEditing && (
              <TableCell className={cellClasses.root}>
                <Checkbox checked={checked} onClick={() => onToggleRow(email, row.date)} />
              </TableCell>
            )}
            <TableCell className={cellClasses.root}>{name}</TableCell>
            <TableCell className={cellClasses.root}>{email}</TableCell>
          </TableRow>
        ))}

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
                      <DeleteEmployeeModalContent
                        date={row.date}
                        onDeleteEmployee={onDeleteEmployee}
                      />
                    );
                  }}
                >
                  Remove People
                </Button>
                <Button
                  variant="contained"
                  color="secondary"
                  onClick={() => {
                    handleModalOpen();
                    setModalState(true);
                    setSelected(
                      <AddEmployeeModalContent
                        users={users}
                        onAddEmployee={onAddEmployee}
                        site={row.office}
                        date={row.date}
                      />
                    );
                  }}
                >
                  Add people
                </Button>
              </HorizontalStack>
            </TableCell>
          </TableRow>
        ) : null}
      </TableBody>
    </Table>
  );
}
