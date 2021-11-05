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
            _status.Background = Brushes.Yellow;
            var (_, _, pDictionary) = run(parse(_codeInput.Text));
            var k = pDictionary[outputBuffer];
            var task = new Task<string>(() => k);
            
            task.ContinueWith(t =>
                Dispatcher.UIThread.Post(() =>
                {
                    try
                    {
                        _console.Text += t.Result + "\n";
                        _status.Background = Brushes.LawnGreen;
                        _status.Text = "Computed successfully!";
                    }
                    catch (Exception exception)
                    {
                        _status.Background = Brushes.Red;
                        _status.Text = exception.Message;
                    }
                    
                }));
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
