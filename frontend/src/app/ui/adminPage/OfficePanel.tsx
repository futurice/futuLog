import React, { useState, useCallback, useContext } from "react";
import { useMutation } from "react-query";

import { useTableContainerStyles, useTableHeadCellStyles, TableEmptyContainer, useRowStyles } from "./CollapsibleTable";
import { officeBookingsQueryKey } from "../../utils/reactQueryUtils";
import { useServices } from "../../services/services";
import {
  IOfficeDto,
} from "../../services/apiClientService";
import { ITableDataDto, ICollapsibleTableHead } from "./types";
import { ModalContext } from "app/providers/ModalProvider";
import { AdminEditContext } from "./AdminEditContext";
import { TableContainer, Paper, Table, TableHead, TableRow, TableCell, TableBody, Button } from "@material-ui/core";
import { HorizontalStack } from "../ux/containers";
import { Checkbox } from "../ux/checkbox";
import { TextField } from "../ux/inputs";

interface IOfficePanel {
  offices: IOfficeDto[];
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
      title: "Capacity",
      width: "30%",
    },
    {
      title: isEditing ? "Exit edit mode" : "Edit list",
      width: "10%",
      onClick: toggleIsEditing,
    },
  ];

export function OfficePanel({ offices }: IOfficePanel) {
  const { apiClient, queryCache } = useServices();

  const { handleModalClose } = useContext(ModalContext);

  const [rows, setRows] = useState<(IOfficeDto & { checked: boolean })[]>(offices as any);
  const [isEditing, setIsEditing] = useState(false);

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

  const tableContainerClasses = useTableContainerStyles();
  const tableHeadClasses = useTableHeadCellStyles();
  const classes = useRowStyles();

  return (
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
                  <TableCell key="capacity">
                    {isEditing ?
                      <HorizontalStack spacing="0.8rem">
                        <TextField value={row.capacity} />
                        <Button variant="contained" color="primary" onClick={() => {}}>Confirm</Button>
                      </HorizontalStack>
                      : row.capacity}
                  </TableCell>
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
                    /*handleModalOpen();
                    setSelected(
                      <DeleteEmployeeModalContent
                        date={row.date}
                        onDeleteEmployee={onDeleteEmployee}
                      />
                    );*/
                  }}
                >
                  Remove offices
                </Button>
                <Button
                  variant="contained"
                  color="secondary"
                  onClick={() => {
                    /*handleModalOpen();
                    setModalState(true);
                    setSelected(
                      <AddEmployeeModalContent
                        users={users}
                        onAddEmployee={onAddEmployee}
                        site={row.office}
                        date={row.date}
                      />
                    );*/
                  }}
                >
                  Add an office
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
  );
}
