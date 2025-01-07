package cz.bbn.cerberus.commons.component.ui.appcard;

import com.github.appreciated.apexcharts.ApexCharts;
import com.github.appreciated.apexcharts.ApexChartsBuilder;
import com.github.appreciated.apexcharts.config.builder.ChartBuilder;
import com.github.appreciated.apexcharts.config.chart.Type;
import com.github.appreciated.apexcharts.config.series.SeriesType;
import com.github.appreciated.apexcharts.helper.Series;
import cz.bbn.cerberus.commons.component.ui.interfaces.MapAction;
import cz.bbn.cerberus.translation.Transl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class AppBarGraphCard extends AppDashboardCard {

    private final MapAction<Double> mapAction;

    public AppBarGraphCard(String title, String route, MapAction<Double> mapAction) {
        super(title, route);
        this.mapAction = mapAction;
        setWidth("45%");
        reloadData();
    }

    @Override
    public void reloadData() {
        getContent().removeAll();
        Map<String, Double> map = mapAction.getMap();
        List<Double> valueList = new ArrayList<>();
        List<String> labelList = new ArrayList<>();
        map.forEach((s, aDouble) -> {
            valueList.add(aDouble);
            labelList.add(Transl.get(s));
        });
        Series<Double> series = new Series<>();
        series.setName("");
        series.setData(valueList.toArray(new Double[valueList.size()]));
        series.setType(SeriesType.COLUMN);
        ApexCharts barChart = ApexChartsBuilder.get()
                .withChart(ChartBuilder.get().withType(Type.BAR).build())
                .withLabels(labelList.toArray(new String[labelList.size()]))
                .withSeries(series)
                .build();
        getContent().add(barChart);
    }
}
