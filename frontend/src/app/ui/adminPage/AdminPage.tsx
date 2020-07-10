import React from 'react';
import { Box } from '@material-ui/core';

import { H2Center, P } from '../ux/text';
import { Stack } from '../ux/containers';
import { colors } from '../ux/theme';
import { Tab, Tabs, TabsPanelWrapper, TabsWrapper } from './styled';


export interface ITabPanelProps {
  children?: React.ReactNode;
  index: any;
  value: any;
}

export function TabPanel(props: ITabPanelProps) {
  const { children, value, index, ...other } = props;

  return (
    <div
      role="tabpanel"
      hidden={value !== index}
      id={`tabpanel-${index}`}
      aria-labelledby={`tab-${index}`}
      {...other}
    >
      {value === index && (
        <Box p={3}>
          <P>{children}</P>
        </Box>
      )}
    </div>
  );
}

function a11yProps(index: any) {
  return {
    id: `tab-${index}`,
    'aria-controls': `tabpanel-${index}`,
  };
}


export const AdminPage: React.FC = () => {
  const [value, setValue] = React.useState(0);

  const handleTabChange = (event: React.ChangeEvent<{}>, newValue: number) => {
    setValue(newValue);
  };

  return (
    <Stack
      className="AdminPage"
      display="flex"
      flexDirection="column"
      height="100%"
      p={[]}
      spacing={[]}
    >
      <Stack
        className="AdminPageTitle"
        display="flex"
        flexDirection="column"
        alignItems="center"
        mx="auto"
        p={["0.5rem", "1rem", "2.5rem"]}
        spacing={["0.5rem", "1rem", "2.5rem"]}
      >
        <H2Center>Who was in the office?</H2Center>
      </Stack>
      <TabsWrapper>
        <Tabs
          value={value}
          onChange={handleTabChange}
          aria-label="Admin page tabs"
        >
          <Tab label="Office visits" {...a11yProps(0)} />
          <Tab label="Person tracking" {...a11yProps(1)} />
        </Tabs>
        <TabsPanelWrapper bgColor={`${colors['deep-blue-10']}`}>
          <TabPanel
            value={value}
            index={0}
          >
            Office visits
          </TabPanel>
          <TabPanel
            value={value}
            index={1}
          >
            Person tracking
          </TabPanel>
        </TabsPanelWrapper>
      </TabsWrapper>
    </Stack>
  );
}
