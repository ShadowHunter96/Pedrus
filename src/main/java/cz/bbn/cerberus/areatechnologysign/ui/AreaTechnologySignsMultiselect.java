package cz.bbn.cerberus.areatechnologysign.ui;

import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import cz.bbn.cerberus.areatechnologysign.AreaTechnologySignDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class AreaTechnologySignsMultiselect extends MultiSelectComboBox<AreaTechnologySignDto> {

    private List<AreaTechnologySignDto> areaTechnologySignDtoList;

    public AreaTechnologySignsMultiselect() {
        this.setLabel(Transl.get("Signs"));
    }

    public AreaTechnologySignsMultiselect(List<AreaTechnologySignDto> areaTechnologySignDtoList) {
        this.areaTechnologySignDtoList = areaTechnologySignDtoList;
        this.setLabel(Transl.get("Signs"));
        initComponent();
    }

    public void initComponent(){
        this.setItems(areaTechnologySignDtoList);
        this.setItemLabelGenerator(dto -> {
            String title = "";
            if(dto.getAreaDto() != null) {
                title += dto.getAreaDto().getName();
                if (dto.getTechnologyDto() != null) {
                    title += " - ";
                }
            }
            if(dto.getTechnologyDto() != null){
                title += dto.getTechnologyDto().getName();
            }
            return title;
        });
    }

    public void setAreaTechnologySignDtoList(List<AreaTechnologySignDto> areaTechnologySignDtoList) {
        this.areaTechnologySignDtoList = areaTechnologySignDtoList;
    }
}
