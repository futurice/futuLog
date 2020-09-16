import { createContext } from "react";
import { IUserDto } from "../../services/apiClientService";

interface IAdminEditContext {
  isEditing: boolean;
  onToggleAllRows: (date: string) => void;
  onToggleRow: (email: string, date: string) => void;
  onAddEmployee: (email: string, formatedDate: string, site: string) => void;
  onDeleteEmployee: (site: string, date: string) => void;
  users: IUserDto[];
}

export const AdminEditContext = createContext<IAdminEditContext>({
  isEditing: false,
  onToggleAllRows: (date: string) => {},
  onToggleRow: (email: string, date: string) => {},
  onAddEmployee: (email: string, formatedDate: string, site: string) => {},
  onDeleteEmployee: (site: string, date: string) => {},
  users: [],
});
