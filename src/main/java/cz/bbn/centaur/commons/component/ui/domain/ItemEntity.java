package cz.bbn.cerberus.commons.component.ui.domain;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Entity;
import javax.persistence.Id;

@Getter
@Setter
@AllArgsConstructor
@Entity
@NoArgsConstructor
public class ItemEntity {

    @Id
    private String id;
    private String name;

}
