package cz.bbn.cerberus.employee.dto;

import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.Objects;

@Getter
@Setter
public class EmployeeDto implements Serializable {

    private String id;

    private String firstName;
    private String lastName;
    private String companyEmail;
    private String personalEmail;
    private String companyPhoneNumber;
    private String personalPhoneNumber;
    private String accountNumber;
    private String position;
    private LocalDate startDate;
    private Boolean active;
    private LocalDate dismissDate;
    private UserDto lineManagerUserDto;
    private UserDto superiorUserDto;

    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EmployeeDto that = (EmployeeDto) o;
        return id.equals(that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }


}
