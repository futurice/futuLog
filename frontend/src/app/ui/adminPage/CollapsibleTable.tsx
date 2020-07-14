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
import { colors } from '../ux/theme';


const useRowStyles = makeStyles({
  root: {
    '& > *': {
      borderBottom: 'none',
    },
    '&:nth-child(2n+1)': {
      borderTop: `1px solid ${colors['deep-blue-20']}`
    }
  }
});

const useTableContainerStyles = makeStyles({
  root: {
    borderRadius: 'unset',
    boxShadow: 'none',
    backgroundColor: 'transparent'
  }
});

const useTableHeadCellStyles = makeStyles({
  root: {
    borderTop: `1px solid ${colors['deep-blue-30']}`,
    borderBottom: `1px solid ${colors['deep-blue-20']}`,
  }
});


export interface ICollapsibleTableHead extends TableCellProps {
  title: string
  width?: string
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
        {/* TODO: Edit button will be down here */}
        <TableCell/>
      </TableRow>
      {
        ChildComponent &&
        <TableRow className={classes.root}>
          <TableCell style={{ padding: 0  }}/>
          <TableCell
            style={{ padding: 0 }}
            colSpan={3}
          >
            <Collapse
              in={open}
              timeout="auto"
              unmountOnExit
            >
              <Box marginTop={2} marginBottom={4}>
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
  const tableContainerClasses = useTableContainerStyles();
  const tableHeadClasses = useTableHeadCellStyles();

  return (
    <TableContainer
      className={tableContainerClasses.root}
      component={Paper}
    >
      <Table aria-label="collapsible table">
        <TableHead>
          <TableRow>
            {parentTableHead.map(({ align = 'left', title, width = '' }: ICollapsibleTableHead) =>
              <TableCell
                style={{ width }}
                className={tableHeadClasses.root}
                align={align}
              >{title}</TableCell>
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

