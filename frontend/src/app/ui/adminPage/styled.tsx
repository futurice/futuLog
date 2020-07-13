import React from 'react';
import {
  Box, BoxProps, styled, Theme, withStyles,
  Tab as MuiTab,
  Tabs as MuiTabs,
  TabProps as MuiTabProps
} from '@material-ui/core';

import { colors } from '../ux/theme';


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
  borderColor: colors['deep-blue-10'],
  borderStyle: 'solid',
  textTransform: 'unset',
  opacity: 0.6,
  transition: 'background-color 0.25s cubic-bezier(0.4, 0, 0.2, 1),' +
    ' border-color 0.25s cubic-bezier(0.4, 0, 0.2, 1),' +
    ' opacity 0.25s cubic-bezier(0.4, 0, 0.2, 1)',

  '&.Mui-selected': {
    backgroundColor: colors['deep-blue-10'],
    border: 'none',
    opacity: 1
  },
  '&:nth-of-type(n+1)': {
    marginLeft: '0.25rem'
  },
  '&:focus': {
    boxShadow: `inset 0 0 0px 3px ${colors["deep-blue-50"]}`,
    opacity: 1
  },
  '&:hover': {
    backgroundColor: colors['deep-blue-10'],
    borderColor: colors['deep-blue-80'],
    opacity: 1
  }
});
