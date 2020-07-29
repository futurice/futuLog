import React from 'react';
import { makeStyles, styled } from '@material-ui/core/styles';
import {
  Table,
  TableBody,
  TableRow,
  TableHead,
  TableContainer,
  Paper,
  Collapse,
  Box
} from '@material-ui/core';

import { IconArrowDown, IconArrowUp } from '../ux/icons';
import { IconButton } from '../ux/buttons';
import { colors } from '../ux/theme';
import { P } from '../ux/text';
import { TableCell } from './styled';
import { ICollapsibleTableHead } from './types';
import { mapBookingsForUI } from './common';


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

interface ICollapsibleTableChild {
  childComponent?: React.ElementType;
  childTableHead?: ICollapsibleTableHead[];
}

interface ICollapsibleTableCell extends ICollapsibleTableChild {
  row: ReturnType<typeof mapBookingsForUI>;
  colSpan?: number;
}

interface ICollapsibleTable extends ICollapsibleTableChild {
  parentTableHead: ICollapsibleTableHead[];
  rows: ReturnType<typeof mapBookingsForUI>[];
  empty?: string;
}

const TableEmpty = styled('div')({
  paddingTop: '60px',
  paddingBottom: '60px',
  textAlign: 'center',

  '& > p': {
    color: colors['deep-blue-90'],
    opacity: 0.6,
    fontWeight: 'bold',
  }
});

const TableEmptyContainer = ({ empty }: { empty: string | undefined }): JSX.Element => (
  <TableEmpty>
    <P>{empty}</P>
  </TableEmpty>
)

function Row({
  row,
  childComponent: ChildComponent,
  childTableHead,
  colSpan
}: ICollapsibleTableCell) {
  const [open, setOpen] = React.useState(false);
  const classes = useRowStyles();

  return (
    <>
      <TableRow className={classes.root}>
        <TableCell>
          <IconButton
            aria-label="expand row"
            onClick={() => setOpen(!open)}
          >
            {open ? <IconArrowUp/> : <IconArrowDown/>}
          </IconButton>
        </TableCell>
        {
          Object.values(row).map((value) => (
            (typeof value === 'string' || typeof value === 'number') && <TableCell key={value}>{value}</TableCell>
          ))
        }
        {/* TODO: Edit button will be down here */}
        <TableCell/>
      </TableRow>
      {
        ChildComponent &&
        <TableRow className={classes.root}>
          <TableCell style={{ padding: 0 }}/>
          <TableCell
            style={{ padding: 0 }}
            colSpan={colSpan}
          >
            <Collapse
              in={open}
              timeout="auto"
              unmountOnExit
            >
              <Box
                marginTop={2}
                marginBottom={4}
              >
                <ChildComponent
                  head={childTableHead}
                  row={row}
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
  rows,
  empty
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
            {parentTableHead.map(({ align = 'left', title, width = '' }: ICollapsibleTableHead, i) =>
              <TableCell
                key={title + i}
                style={{ width }}
                className={tableHeadClasses.root}
                align={align}
              >{title}</TableCell>
            )}
          </TableRow>
        </TableHead>
        <TableBody>
          {
            rows.length !== 0 && (
              rows.map((row, i) => (
                <Row
                  key={i + '1'}
                  row={row}
                  childComponent={childComponent}
                  childTableHead={childTableHead}
                  colSpan={parentTableHead.length - 2}
                />
              ))
            )
          }
        </TableBody>
      </Table>
      {
        rows.length === 0 && <TableEmptyContainer empty={empty} />
      }
    </TableContainer>
  );
}
