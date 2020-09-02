import React, { useContext } from "react";
import { Table, TableBody, TableHead, TableRow } from "@material-ui/core";
import { makeStyles } from "@material-ui/core/styles";

import { colors } from "../ux/theme";
import { Checkbox } from "../ux/checkbox";
import { TableCell } from "./styled";
import { ICollapsibleTableHead } from "./types";
import { mapBookingsForUI } from "./OfficeVisitsPanel";
import { EditOfficeVisitsContext } from './OfficeVisitsPanel';
import { Button } from "../ux/buttons";
import { Flex } from "../ux/containers";

import { ModalContext } from '../../providers/ModalProvider';


interface IBookingsTable {
  row: ReturnType<typeof mapBookingsForUI>,
  head: ICollapsibleTableHead[],
  editUserButtons?: boolean;
}

const useChildTableStyles = makeStyles({
  root: {
    boxShadow: "none",
    backgroundColor: "transparent"
  }
})

const useChildCellStyles = makeStyles({
  root: {
    border: "none",
  }
});

const useTableHeadCellStyles = makeStyles({
  root: {
    borderTop: `1px solid ${colors["deep-blue-20"]}`,
    borderBottom: `1px solid ${colors["deep-blue-20"]}`
  }
});


export function BookingsTable({ row, head, editUserButtons }: IBookingsTable) {
  console.log(editUserButtons);
  const tableClasses = useChildTableStyles();
  const cellClasses = useChildCellStyles();
  const headCellClasses = useTableHeadCellStyles();
  const { isEditing, onToggleAllRows, onToggleRow } = useContext(EditOfficeVisitsContext);

  const { handleModalOpen, setSelected, setModalState } = useContext(ModalContext);
  console.log(row);

  return (
    <Table
      className={tableClasses.root}
      size="small"
      aria-label="visitors"
    >
      <TableHead>
        <TableRow>
          {
            head.map(({ align = "left", title, checked }: ICollapsibleTableHead) =>
              <TableCell
                key={title}
                className={headCellClasses.root}
                align={align}
              >
                {checked ? <Checkbox value={checked} onClick={() => onToggleAllRows(row.date)} /> : title}
              </TableCell>
            )}
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
            >{i + 1}</TableCell>
            {/* TODO: when edit button is clicked checkbox column will be displayed */}
            {isEditing && <TableCell className={cellClasses.root}><Checkbox checked={checked} onClick={() => onToggleRow(email, row.date)} /></TableCell>}
            <TableCell className={cellClasses.root}>{name}</TableCell>
            <TableCell className={cellClasses.root}>{email}</TableCell>
          </TableRow>
        ))}

        {editUserButtons && isEditing ?
          (<TableRow>
            <TableCell colSpan={3}>
              <Flex paddingTop="2.5rem">
                <Button
                  variant="contained"
                  color="primary"
                  onClick={handleModalOpen}
                // disabled={if no users selected}
                >
                  Remove People
              </Button>

                <Button
                  variant="outlined"
                  color="primary"
                  onClick={() => {
                    setSelected(row)
                    setModalState(true)
                  }}
                >
                  Add people
              </Button>
              </Flex>
            </TableCell>
          </TableRow>) : null}

      </TableBody>
    </Table>
  )
}
