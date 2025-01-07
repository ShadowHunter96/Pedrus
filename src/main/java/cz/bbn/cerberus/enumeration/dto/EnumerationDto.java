package cz.bbn.cerberus.enumeration.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.util.Objects;

@Getter
@Setter
@ToString
@NoArgsConstructor
public class EnumerationDto implements Serializable {
    private Long id;
    private String name;
    private EnumerationTypeDto enumerationTypeDto;
    private String description;
    private String value;
    private Boolean allowed;
    private Boolean deleted;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        EnumerationDto that = (EnumerationDto) o;
        return id.equals(that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
