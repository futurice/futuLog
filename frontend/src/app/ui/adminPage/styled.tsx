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


export const Tab = styled((props: MuiTabProps) => <MuiTab {...props}/>)({
  boxShadow: 'none',
  borderTopLeftRadius: '8px',
  borderTopRightRadius: '8px',
  borderTopWidth: '1px',
  borderLeftWidth: '1px',
  borderRightWidth: '1px',
  borderColor: colors['deep-blue-10'],
  borderStyle: 'solid',
  textTransform: 'unset',

  '&.Mui-selected': {
    backgroundColor: colors['deep-blue-10'],
    border: 'none'
  },
  '&:nth-of-type(n+1)': {
    marginLeft: '0.25rem'
  }
});
