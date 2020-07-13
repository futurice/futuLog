import React from 'react';
import { makeStyles } from '@material-ui/core/styles';
import {
  Table,
  TableBody,
  TableCell,
  TableRow,
  TableHead,
  TableContainer,
  Paper,
  Collapse,
  Box
} from '@material-ui/core';
import { TableCellProps } from '@material-ui/core/TableCell/TableCell';

import { createData } from './OfficeVisits';
import { IconArrowDown, IconArrowUp } from '../ux/icons';
import { IconButton } from '../ux/buttons';


const useRowStyles = makeStyles({
  root: {
    '& > *': {
      borderBottom: 'unset',
    },
  },
});

const useTableStyles = makeStyles({
  root: {
    boxShadow: 'none',
    backgroundColor: 'transparent' // TODO
  }
})


export interface ICollapsibleTableHead extends TableCellProps {
  title: string
}

export interface ICollapsibleTableChild {
  childComponent?: React.ElementType,
  childTableHead?: ICollapsibleTableHead[]
}

export interface ICollapsibleTable extends ICollapsibleTableChild {
  parentTableHead: ICollapsibleTableHead[],
  rows: ReturnType<typeof createData>[]
}

export interface ICollapsibleTableCell extends ICollapsibleTableChild {
  row: ReturnType<typeof createData>
}


function Row({
  row,
  childComponent: ChildComponent,
  childTableHead
}: ICollapsibleTableCell) {
  const [open, setOpen] = React.useState(false);
  const classes = useRowStyles();

  return (
    <>
      <TableRow className={classes.root}>
        <TableCell>
          <IconButton
            aria-label="expand row"
            size="small"
            onClick={() => setOpen(!open)}
          >
            {open ? <IconArrowUp/> : <IconArrowDown/>}
          </IconButton>
        </TableCell>
        <TableCell>{row.date}</TableCell>
        <TableCell>{row.office}</TableCell>
        <TableCell>{row.capacityUtilisation}</TableCell>
      </TableRow>
      {
        ChildComponent &&
        <TableRow>
          <TableCell
            style={{ paddingBottom: 0, paddingTop: 0 }}
            colSpan={6}
          >
            <Collapse
              in={open}
              timeout="auto"
              unmountOnExit
            >
              <Box margin={1}>
                <ChildComponent
                  head={childTableHead || []}
                  row={row || []}
                />
              </Box>
            </Collapse>
          </TableCell>
        </TableRow>
      }
    </>
  );
}

export default function CollapsibleTable({
  childComponent,
  childTableHead,
  parentTableHead,
  rows
}: ICollapsibleTable) {
  const classes = useTableStyles();

  return (
    <TableContainer component={Paper}>
      <Table className={classes.root} aria-label="collapsible table">
        <TableHead>
          <TableRow>
            <TableCell/>
            {parentTableHead.map(({ align='left', title }: ICollapsibleTableHead) =>
              <TableCell align={align}>{title}</TableCell>
            )}
          </TableRow>
        </TableHead>
        <TableBody>
          {rows.map((row) => (
            <Row
              key={row.date}
              row={row}
              childComponent={childComponent}
              childTableHead={childTableHead}
            />
          ))}
        </TableBody>
      </Table>
    </TableContainer>
  );
}

