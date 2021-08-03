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
import { TableContainer, Paper, Table, TableHead, TableRow, TableCell, TableBody } from "@material-ui/core";

interface IOfficePanel {
  offices: IOfficeDto[];
}

const tableHead = (
  isEditing: boolean,
  toggleIsEditing: () => void
): ICollapsibleTableHead[] => [
    {
      title: "Name",
      width: "60%",
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

  const [rows, setRows] = useState<IOfficeDto[]>(offices);
  const [isEditing, setIsEditing] = useState(false);
  const [selectAll, setSelectAll] = useState(false);

  const toggleIsEditing = useCallback(() => {
    setIsEditing(!isEditing);
  }, [isEditing, setIsEditing]);

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
              >{title}</TableCell>
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
                  <TableCell key="name">{row.name}</TableCell>
                  <TableCell key="capacity">{row.capacity}</TableCell>
                </TableRow>
              ))
            )
          }
        </TableBody>
      </Table>
      {
        rows.length === 0 && <TableEmptyContainer empty={"No result for the selected parameters."} />
      }
    </TableContainer>
  );
}
