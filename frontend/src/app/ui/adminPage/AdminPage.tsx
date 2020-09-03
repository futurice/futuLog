import React from "react";
import { useQuery } from "react-query";
import { Box } from "@material-ui/core";
import { H2Center } from "../ux/text";
import { Stack } from "../ux/containers";
import { colors } from "../ux/theme";
import { Tab, Tabs, TabsPanelWrapper, TabsWrapper } from "./styled";
import { IOfficeSpaceDto } from "../../services/apiClientService";
import { OfficeVisitsPanel } from "./OfficeVisitsPanel";
import { PersonTrackingPanel } from "./PersonTrackingPanel";
import { usersQueryKey } from "../../utils/reactQueryUtils";
import { useServices } from "../../services/services";
import { CenteredSpinner } from "../ux/spinner";

export interface ITabPanelProps {
  children?: React.ReactNode;
  index: any;
  value: any;
}

export interface IAdminPage {
  offices: IOfficeSpaceDto[];
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
          {children}
        </Box>
      )}
    </div>
  );
}

function a11yProps(index: any) {
  return {
    id: `tab-${index}`,
    "aria-controls": `tabpanel-${index}`,
  };
}


export const AdminPage: React.FC<IAdminPage> = ({ offices }) => {
  const { apiClient: apiClientService } = useServices();
  const { data: users, isFetching } = useQuery(usersQueryKey(), () => apiClientService.getUsers());

  const [tab, setTab] = React.useState(0);

  const handleTabChange = (event: React.ChangeEvent<{}>, newValue: number) => {
    setTab(newValue);
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
      {
        isFetching ? (
          <CenteredSpinner />
        ) : (
            <>
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
                  value={tab}
                  onChange={handleTabChange}
                  aria-label="Admin page tabs"
                >
                  <Tab label="Office visits" {...a11yProps(0)} />
                  <Tab label="Person tracking" {...a11yProps(1)} />
                </Tabs>
                <TabsPanelWrapper bgColor={`${colors["deep-blue-10"]}`}>
                  <TabPanel
                    value={tab}
                    index={0}
                  >
                    <OfficeVisitsPanel offices={offices} users={users || []} />
                  </TabPanel>
                  <TabPanel
                    value={tab}
                    index={1}
                  >
                    <PersonTrackingPanel
                      users={users || []}
                      offices={offices || []}
                    />
                  </TabPanel>
                </TabsPanelWrapper>
              </TabsWrapper>
            </>
          )
      }
    </Stack>
  );
}
