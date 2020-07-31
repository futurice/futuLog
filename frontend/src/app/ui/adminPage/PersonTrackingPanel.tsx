/*
* TODO WIP: to be able to send user and dates and get a response in the following
* TODO WIP: format [{ date: '', people: []}], like it's done in OfficeVisitsPanel.tsx
* */

import React, { useMemo, useState } from 'react';
import { queryCache, useMutation } from 'react-query';
import dayjs from 'dayjs';

import CollapsibleTable from './CollapsibleTable';
import { userBookingsQueryKey, userContactsQueryKey } from '../../utils/reactQueryUtils';
import { useServices } from '../../services/services';
import {
  ICapacityDto,
  IOfficeSpaceDto, IUserBookingsDto,
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
  site,
  user,
  offices
}: {
  bookings: ICapacityDto,
  site: string,
  user: string,
  offices: IOfficeSpaceDto[]
}): ITableDataDto => {
  const { people, date } = bookings;
  const mappedPeople: IUserDtoMapped[] = people.map((person: IUserDto) => ({
    name: `${person.first_name} ${person.last_name}`,
    email: person.email
  }));
  const { maxPeople } = offices.filter((office) => office.site === site)[0];

  return {
    date: date,
    site,
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
    mutateUserBookings({
      user: currentUser,
      startDate: startDate.format('YYYY-MM-DD'),
      endDate: startDate.subtract(range, 'day').format('YYYY-MM-DD')
    });
    mutateUserContacts({
      user: currentUser,
      startDate: startDate.format('YYYY-MM-DD'),
      endDate: startDate.subtract(range, 'day').format('YYYY-MM-DD')
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

  const [mutateUserBookings, mutateUserBookingsRes] = useMutation(
    ({ user, startDate, endDate }: IUserDataRequestDto) => apiClient.getUserBookings({ user, startDate, endDate }),
    {
      onSuccess: () => queryCache.refetchQueries(userBookingsQueryKey(currentUser, startDateStr, endDateStr)),
    }
  );

  const rows: ITableDataDto[] = useMemo(() => {
    const { data: userBookings } = mutateUserBookingsRes;
    const { data } = mutateUserContactsRes;
    const user = users.filter(({ email }) => currentUser === email)[0];

    const mappedBookings: ITableDataDto[] | undefined = data && data.map(
      (item: ICapacityDto) => {
        const { site } = (userBookings || []).filter((booking: IUserBookingsDto) => {
          return booking.date === item.date;
        })[0];

        return mapBookingsForUI({ bookings: item, site, user: `${user?.first_name} ${user?.last_name}`, offices: offices || [] });
      });

    return mappedBookings || [];
  }, [mutateUserContactsRes, mutateUserBookingsRes]);

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
