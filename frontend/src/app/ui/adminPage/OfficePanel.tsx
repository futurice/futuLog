import React, { useState, useCallback, useContext } from "react";
import { useMutation, useQuery } from "react-query";

import { useTableContainerStyles, useTableHeadCellStyles, TableEmptyContainer } from "./CollapsibleTable";
import { officeBookingsQueryKey, officesQueryKey, RenderQuery } from "../../utils/reactQueryUtils";
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


export function OfficePanel({ offices: initialOffices }: IOfficePanel) {
  const { apiClient, queryCache } = useServices();

  const { handleModalClose, handleModalOpen, setSelected } = useContext(ModalContext);

  const [rows, setRows] = useState<(IOfficeDto & { checked: boolean })[]>(initialOffices as any);
  const [isEditing, setIsEditing] = useState(false);
  const [capacityEditing, setCapacityEditing] = useState<number | undefined>(undefined);
  const [capacity, setCapacity] = useState<string>("");

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

  const [updateCapacity] = useMutation(
    (request: IOfficeDto) => apiClient.admin.setOffice(request),
    {
      onSuccess: () => queryCache.refetchQueries(officesQueryKey())
    }
  );
  const [deleteOffices] = useMutation(
    async (requests: string[]) => {
      for(const r of requests) {
        await apiClient.admin.deleteOffice(r);
      }
    },
    {
      onSuccess: () => queryCache.refetchQueries(officesQueryKey())
    }
  );

  const officesRes = useQuery(officesQueryKey(), () =>
    apiClient.getOffices().catch(() => null).then(offices => {
      setRows(offices as any)
      return offices;
    })
  );

  const confirmCapacity = (name: string) => () => {
    const x = parseInt(capacity);
    if(capacity !== undefined && !isNaN(x)) {
      updateCapacity({
        name,
        capacity: x
      });
    }
    setCapacityEditing(undefined);
  };

  const tableContainerClasses = useTableContainerStyles();
  const tableHeadClasses = useTableHeadCellStyles();
  const classes = useRowStyles();

  const onDeleteOffices = () => {
    const names = rows.filter(x => x.checked).map(x => x.name);
    deleteOffices(names);
    handleModalClose();
  };

  return (
    <RenderQuery
      query={officesRes}
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
                  <TableCell key="capacity">
                    {capacityEditing === i ?
                      <HorizontalStack spacing="0.8rem">
                        <TextField value={capacity} onInput={(ev: any) => setCapacity(ev.target.value)} />
                        <Button variant="contained" color="primary" onClick={confirmCapacity(row.name)} style={{ marginLeft: "0.8rem" }}>Confirm</Button>
                      </HorizontalStack>
                      : <>
                        {row.capacity}
                        {isEditing && <Button variant="contained" disabled={capacityEditing !== undefined} color="primary" onClick={() => {
                          setCapacityEditing(i);
                          setCapacity(String(row.capacity));
                        }} style={{ marginLeft: "0.8rem" }}>Edit</Button>}
                      </>}
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
                    handleModalOpen();
                    setSelected(
                      <DeleteOfficeModalContent
                        onDelete={onDeleteOffices}
                      />
                    );
                  }}
                >
                  Remove offices
                </Button>
                <Button
                  variant="contained"
                  color="secondary"
                  onClick={() => {
                    handleModalOpen();
                    setSelected(
                      <AddOfficeModalContent
                        onAdd={(name, capacity) => {
                          if(!isNaN(capacity)) {
                            updateCapacity({ name, capacity });
                          }
                          }}
                      />
                    );
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
    }</RenderQuery>
  );
}
