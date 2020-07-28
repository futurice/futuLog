import { ICapacityDto, IUserDto } from '../../services/apiClientService';
import { ITableDataDto, IUserDtoMapped } from './types';

export function mapBookingsForUI({
  bookings,
  site
}: {
  bookings: ICapacityDto,
  site: string
}): ITableDataDto {
  const { people, date } = bookings;
  const mappedPeople: IUserDtoMapped[] = people.map((person: IUserDto) => ({
    name: `${person.first_name} ${person.last_name}`,
    email: person.email
  }));

  return {
    date: date,
    site,
    visitors: mappedPeople,
    utilisation: mappedPeople.length
  };
}
