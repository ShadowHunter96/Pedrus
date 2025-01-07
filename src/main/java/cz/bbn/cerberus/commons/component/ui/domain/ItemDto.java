package cz.bbn.cerberus.commons.component.ui.domain;

import cz.bbn.cerberus.user.dto.UserDto;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.Objects;
import java.util.UUID;

@Getter
@Setter
@AllArgsConstructor
public class ItemDto implements Serializable {

    private String id;
    private String name;

    public ItemDto(String name) {
        this.id = UUID.randomUUID().toString();
        this.name = name;
    }

    public ItemDto(UserDto userDto) {
        this.id = userDto.getLogin();
        this.name = userDto.getName();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ItemDto itemDto = (ItemDto) o;
        return id.equals(itemDto.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
