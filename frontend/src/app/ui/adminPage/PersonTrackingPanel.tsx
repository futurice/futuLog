import React, { useMemo, useState } from 'react';
import { queryCache, useMutation } from 'react-query';
import dayjs from 'dayjs';

import CollapsibleTable from './CollapsibleTable';
import { userContactsQueryKey } from '../../utils/reactQueryUtils';
import { useServices } from '../../services/services';
import {
  ICapacityDto,
  IOfficeSpaceDto,
  IUserDataRequestDto,
  IUserDto
} from '../../services/apiClientService';
import { ICollapsibleTableHead, ITableDataDto, IUserDtoMapped } from './types';
import { TrackingToolbar } from './TrackingToolbar';
import { BookingsTable } from './BookingsTable';
import { CenteredSpinner, CenteredSpinnerContainer } from '../ux/spinner';


interface IPersonTrackingPanel {
  users: IUserDto[];
  offices?: IOfficeSpaceDto[];
}

const childTableHead: ICollapsibleTableHead[] = [
  {
    title: 'nr.'
  },
  {
    title: 'Name'
  },
  {
    title: 'Email'
  }
];

const parentTableHead: ICollapsibleTableHead[] = [
  {
    title: '',
    width: '5%'
  },
  {
    title: 'Date',
    width: '10%'
  },
  {
    title: 'Office',
    width: '10%'
  },
  {
    title: 'Person',
    width: '15%'
  },
  {
    title: 'Capacity utilisation',
    width: '10%'
  },
  {
    title: '',
    width: '50%'
  }
];

const mapBookingsForUI = ({
  bookings,
  user,
  offices
}: {
  bookings: ICapacityDto,
  user: string,
  offices: IOfficeSpaceDto[]
}): ITableDataDto => {
  const { people, date, site } = bookings;
  const mappedPeople: IUserDtoMapped[] = people.map((person: IUserDto) => ({
    name: `${person.first_name} ${person.last_name}`,
    email: person.email
  }));
  const { maxPeople } = offices.filter((office) => office.site === site)[0];

  return {
    date: date,
    site: site || '',
    person: user,
    visitors: mappedPeople,
    utilisation: `${mappedPeople.length} people (max ${maxPeople})`
  };
}

export function PersonTrackingPanel({
  users,
  offices
}: IPersonTrackingPanel) {
  const { apiClient } = useServices();
  const today = dayjs().utc().startOf('day');

  const [startDate, setStartDate] = useState(() => today.startOf('week').add(1, 'day'));
  const [endDate, setEndDate] = useState(() => today.startOf('week').add(1, 'day'));
  const [currentUser, setCurrentUser] = React.useState('');
  const [range, setRange] = React.useState(0);

  const startDateStr = startDate.format('YYYY-MM-DD');
  const endDateStr = endDate.format('YYYY-MM-DD');

  const handleSearch = () => {
    mutateUserContacts({
      user: currentUser,
      startDate: startDate.subtract(range, 'day').format('YYYY-MM-DD'),
      endDate: startDate.format('YYYY-MM-DD')
    });
  }

  const handleUserChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    const userEmail = event.target.value as string;
    setCurrentUser(userEmail);
  }

  const handleRangeChange = (event: React.ChangeEvent<{ name?: string; value: unknown }>) => {
    setRange(event.target.value as number);
  }

  const handleDateChange = (startDate: dayjs.Dayjs, endDate: dayjs.Dayjs) => {
    setStartDate(startDate);
    setEndDate(endDate);
  }

  const [mutateUserContacts, mutateUserContactsRes] = useMutation(
    ({ user, startDate, endDate }: IUserDataRequestDto) => apiClient.getUserContacts({ user, startDate, endDate }),
    {
      onSuccess: () => queryCache.refetchQueries(userContactsQueryKey(currentUser, startDateStr, endDateStr)),
    }
  );

  const rows: ITableDataDto[] = useMemo(() => {
    const { data } = mutateUserContactsRes;
    const user = users.filter(({ email }) => currentUser === email)[0];

    const mappedBookings: ITableDataDto[] | undefined = data && data.map(
      (item: ICapacityDto) => {
        return mapBookingsForUI({ bookings: item, user: `${user?.first_name} ${user?.last_name}`, offices: offices || [] });
      });

    return mappedBookings || [];
  }, [mutateUserContactsRes]);

  return (
    <>
      <TrackingToolbar
        tableData={rows}
        startDate={startDate}
        range={range}
        users={users}
        currentUser={currentUser}
        onDateChange={handleDateChange}
        onUserChange={handleUserChange}
        onRangeChange={handleRangeChange}
        onSearch={handleSearch}
      />
      {
        mutateUserContactsRes.status === 'loading' ? (
          <CenteredSpinnerContainer style={{ minHeight: '250px' }}>
            <CenteredSpinner/>
          </CenteredSpinnerContainer>
        ) : (
          <CollapsibleTable
            childComponent={BookingsTable}
            childTableHead={childTableHead}
            parentTableHead={parentTableHead}
            empty={'No result for the selected parameters.'}
            rows={rows}
          />
        )
      }
    </>
  );
}
