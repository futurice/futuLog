import React from 'react';
import {
  Box, BoxProps, styled, Theme, withStyles,
  Tab as MuiTab,
  Tabs as MuiTabs,
  TabProps as MuiTabProps, TableCell as MuiTableCell
} from '@material-ui/core';

import { colors } from '../ux/theme';
import { Flex } from '../ux/containers';
import { TableCellProps as MuiTableCellProps } from '@material-ui/core/TableCell/TableCell';


interface ITabsPanelWrapper extends BoxProps {
  bgColor: string
}

interface ICustomTabsProps {
  value: number;
  onChange: (event: React.ChangeEvent<{}>, newValue: number) => void;
}

export const TabsWrapper = styled('div')({
  display: 'flex',
  flexDirection: 'column',
  flex: 1
});

export const TabsPanelWrapper = styled(({ bgColor, ...props }) => <Box {...props} />)<Theme,
  ITabsPanelWrapper>(({ theme, bgColor }) => ({
  backgroundColor: bgColor,
  flex: 1
}));

export const Tabs = withStyles({
  root: {
    paddingLeft: '1.5rem',
  },
  indicator: {
    display: 'none'
  },
})((props: ICustomTabsProps) => <MuiTabs {...props} TabIndicatorProps={{ children: <span/> }}/>);


export const Tab = styled((props: MuiTabProps) => <MuiTab {...props} disableRipple/>)({
  boxShadow: 'none',
  borderTopLeftRadius: '8px',
  borderTopRightRadius: '8px',
  borderTopWidth: '1px',
  borderLeftWidth: '1px',
  borderRightWidth: '1px',
  borderColor: colors['deep-blue-20'],
  borderStyle: 'solid',
  textTransform: 'unset',
  color: colors['deep-blue-40'],
  opacity: 1,
  transition: 'background-color 0.25s cubic-bezier(0.4, 0, 0.2, 1),' +
    ' box-shadow 0.25s cubic-bezier(0.4, 0, 0.2, 1),' +
    ' color 0.25s cubic-bezier(0.4, 0, 0.2, 1)',

  '&.Mui-selected': {
    backgroundColor: colors['deep-blue-10'],
    border: 'none',
    color: colors['deep-blue-80'],
  },
  '&:nth-of-type(n+1)': {
    marginLeft: '0.25rem'
  },
  '&:hover': {
    color: colors['deep-blue-80'],
    boxShadow: `2px 2px 4px rgba(10, 3, 37, 0.2)`
  },
  '&:disabled': {
    color: colors['deep-blue-20'],
    borderColor: colors['deep-blue-10']
  }
});

export const Toolbar = styled(Flex)({
  marginBottom: '35px',
  marginTop: '5px',
  alignItems: 'flex-end'
});

export const ToolbarItem = styled(Flex)({
  flexDirection: 'column',
  alignItems: 'flex-start',

  '&:nth-child(n+2)': {
    marginLeft: '40px'
  },
  '&:last-child': {
    marginLeft: 'auto',
  },
  '& > button:first-child': {
    marginLeft: '50px'
  },
  '& > * + div': {
    marginTop: '10px'
  },
  '& .MuiFormControl-root': {
    marginLeft: '0',
    marginRight: '0',
    marginBottom: '0'
  }
});

export const TableCell = styled((props: MuiTableCellProps) => <MuiTableCell {...props} />)({
  color: colors['deep-blue-90'],
});
