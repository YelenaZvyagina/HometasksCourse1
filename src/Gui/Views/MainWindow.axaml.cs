using System;
using System.IO;
using System.Threading.Tasks;
using Avalonia;
using Avalonia.Controls;
using Avalonia.Interactivity;
using Avalonia.Markup.Xaml;
using Avalonia.Media;
using Avalonia.Threading;
using static Interpreter;


namespace Gui.Views
{
    public partial class MainWindow : Window
    {
        
        private string _path;
        private readonly TextBox _codeInput;
        private readonly TextBlock _console;
        private readonly TextBlock _status;
        private readonly MenuItem _run;

        public MainWindow()
        {
            InitializeComponent();
            _codeInput = this.Find<TextBox>( "CodeInput");
            _console = this.Find<TextBlock>( "Console");
            _run = this.FindControl<MenuItem>("Run");
            _status = this.FindControl<TextBlock>("Status");
            _path = "";

        }

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }
        

        public async void Open(object? sender, RoutedEventArgs e)
        {
            var dialog = new OpenFileDialog();
            dialog.Filters.Add(new FileDialogFilter { Extensions = { "txt" } });
            var path = await dialog.ShowAsync(this);
            if (path is not {Length: > 0}) return;
            _codeInput.Text = await File.ReadAllTextAsync(path[0]);
            _path = path[0];
        }

        public void New(object? sender, RoutedEventArgs e)
        {
            _codeInput.Text = "";
            _path = "";
            Save(sender, e);
        }

        private void Run(object? sender, RoutedEventArgs e)
        {
            _status.Text = "loading..";
            _run.IsEnabled = false;
            var resStr = "";
            var task = new Task<(string, string)>(() =>
            {
                try
                {
                    var (_, _, pDictionary) = run(parse(_codeInput.Text));
                    var k = pDictionary[outputBuffer];
                    resStr += k;
                    return ("Computed successfully!", resStr);
                }
                catch (Exception ex)
                {
                    return (ex.Message, resStr);
                }
            });
            task.ContinueWith(x => 
                Dispatcher.UIThread.Post(() =>
                {
                    var (status, res) = x.Result;
                    _status.Text = status;
                    _console.Text = res;
                    _run.IsEnabled = true;
                })
            );
            
            task.Start();
        }


        public async void Save(object? sender, RoutedEventArgs e)
        {
            var dialog = new SaveFileDialog
            {
                InitialFileName = _path
            };
            var path = await dialog.ShowAsync(this);
            if (path == null) return;
            await File.WriteAllTextAsync(path, _codeInput.Text);
            _path = path;
        }
    }
  
}
