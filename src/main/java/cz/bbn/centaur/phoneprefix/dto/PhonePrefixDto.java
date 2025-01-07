package cz.bbn.cerberus.phoneprefix.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class PhonePrefixDto implements Serializable {

    private String countryCode;
    private String phonePrefix;

}
